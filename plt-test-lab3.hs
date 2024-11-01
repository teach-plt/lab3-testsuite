{-# LANGUAGE CPP #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- GHC needs -threaded

import Control.Exception      ( IOException, catch )
import Control.Monad          ( forM, forM_, unless, void, when )
import Control.Monad.Reader   ( ReaderT, runReaderT, asks )
import Control.Monad.IO.Class ( liftIO )

import Data.Char              ( isSpace )
import Data.List              ( isInfixOf, partition, sort )

import System.Console.GetOpt  ( OptDescr(Option), pattern RequireOrder, pattern NoArg, pattern ReqArg, getOpt )
import System.Directory       ( doesFileExist, doesDirectoryExist, exeExtension, listDirectory, removeFile )
import System.Environment     ( getArgs, lookupEnv )
import System.FilePath        ( (<.>), replaceExtension, takeBaseName, takeDirectory, takeExtension )
import System.Exit            ( ExitCode(ExitFailure, ExitSuccess), exitFailure, exitSuccess )
import System.IO              ( Handle, pattern LineBuffering, hIsTerminalDevice, hSetBuffering
                              , hPutStr, hPutStrLn, stderr, stdout )
import System.IO.Unsafe       ( unsafePerformIO )
import System.Process         ( readProcessWithExitCode, showCommandForUser )

---------------------------------------------------------------------------
-- * Configuration
---------------------------------------------------------------------------

type TestSuite = [FilePath]

-- | When no @test@ option is given.
defaultTestSuite :: TestSuite
defaultTestSuite = [ "lab2-testsuite/good", "dir-for-path-test/one-more-dir" ]

-- | Executable name.
executable_name :: FilePath
-- You might have to add or remove .exe here if you are using Windows
executable_name = "lab3" <.> exeExtension

classpathSep :: Char
#if defined(mingw32_HOST_OS)
classpathSep = ';'
#else
classpathSep = ':'
#endif

-- | Use slash as path separator also under Windows.
(</>) :: FilePath -> FilePath -> FilePath
x </> y = concat [ x, "/", y ]

data Options = Options
  { debugFlag       :: Bool
  , doublesFlag     :: Bool
  , makeFlag        :: Bool
  , testSuiteOption :: TestSuite
  , progDir         :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { debugFlag       = False
  , doublesFlag     = True
  , makeFlag        = True
  , testSuiteOption = []
  , progDir         = ""
  }

---------------------------------------------------------------------------
-- * Main
---------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering -- in various contexts this is guessed incorrectly
  args <- getArgs
  opts <- maybe usage pure $ parseArgs args
  runReaderT mainOpts opts

usage :: IO a
usage = do
  hPutStrLn stderr "Usage: plt-test-lab3 [--debug] [--no-doubles] [--no-make] [-t|--test DIRECTORY]..."
  hPutStrLn stderr "           compiler_code_directory"
  exitFailure

parseArgs :: [String] -> Maybe Options
parseArgs argv =
  case getOpt RequireOrder optDescr argv of
    (o,[progdir],[]) -> do
      let options   = foldr ($) defaultOptions o
      let testSuite = replaceNull (testSuiteOption options) defaultTestSuite
      return options{ testSuiteOption = testSuite, progDir = progdir }
    (_,_,_) -> Nothing

  where
    optDescr :: [OptDescr (Options -> Options)]
    optDescr = [ Option []    ["debug"]      (NoArg  enableDebug       ) "print debug messages"
               , Option []    ["doubles"]    (NoArg  enableDoubles     ) "include double tests"  -- default
               , Option []    ["no-doubles"] (NoArg  disableDoubles    ) "exclude double tests"
               , Option []    ["no-make"]    (NoArg  disableMake       ) "do not run make"
               , Option ['t'] ["test"]       (ReqArg addTest     "FILE") "good test case FILE"   -- many
               ]

    enableDebug :: Options -> Options
    enableDebug options = options { debugFlag = True }

    enableDoubles :: Options -> Options
    enableDoubles options = options { doublesFlag = True }

    disableDoubles :: Options -> Options
    disableDoubles options = options { doublesFlag = False }

    disableMake :: Options -> Options
    disableMake options = options { makeFlag = False }

    addTest :: FilePath -> Options -> Options
    addTest f options = options { testSuiteOption = f : testSuiteOption options }

type M = ReaderT Options IO

type Tests = [FilePath]

mainOpts :: M ()
mainOpts = do
  liftIO $ putStrLn "This is the test program for Programming Languages Lab 3"

  -- Compute testsuite from directories
  testSuite <- asks testSuiteOption
  tests <- concat <$> do
    forM testSuite \ f -> do
      -- Expand each directory into its files.
      liftIO (doesDirectoryExist f) >>= \case
        True -> listCCFiles f
        False -> pure [f]

  -- Cleanup files from old runs
  forM_ tests \ f -> liftIO do
    cleanFiles $ map (replaceExtension f) [".j", ".class"]

  -- Build the compiler
  progdir <- asks progDir
  whenM (asks makeFlag) $ runMake progdir

  -- Test the compiler
  good <- liftIO $ runTests progdir tests

  -- Report the results
  liftIO do
    putStrLn ""
    putStrLn "------------------------------------------------------------"
    ok <- report "Good programs: " good
    if ok then exitSuccess else exitFailure

---------------------------------------------------------------------------
-- * Test driver
---------------------------------------------------------------------------

-- | Run "make" in given directory.
runMake :: FilePath -> M ()
runMake dir = do
  liftIO $ checkDirectoryExists dir
  runPrgNoFail_ "make" ["-C"] dir

-- | Run test on all ".cc" files in given directories (default "good").
runTests :: FilePath -> Tests -> IO [(FilePath,Bool)]
runTests dir files = do
  let prog = dir </> executable_name
  checkFileExists prog
  mapM (\ f -> (f,) <$> testBackendProg prog f) files

-- | Test given program on given test file.
testBackendProg
  :: FilePath  -- ^ Program
  -> FilePath  -- ^ Test file
  -> IO Bool
testBackendProg prog f = do
  input  <- readFileIfExists (f ++ ".input")
  output <- readFileIfExists (f ++ ".output")

  -- Running prog on f should generate file f.class
  putStr $ "Running " ++ f ++ "... "
  (compilerRet, compilerOut, compilerErr) <- readProcessWithExitCode prog [f] ""
  putStrLnExitCode compilerRet "."
  if isExitFailure compilerRet then do
    reportError prog "non-zero exit code" (Just f) (nullMaybe input) (nullMaybe compilerOut) (nullMaybe compilerErr)
    return False
  else do
    let expectedJavaClassFilePath = replaceExtension f ".class"
    javaClassFileCreated <- doesFileExist expectedJavaClassFilePath
    if javaClassFileCreated then do
      -- Run code
      -- A. Abel, 2018-11-26: put test file directory first in classpath.
      -- This avoids problems if there are stale .class files
      -- in the directory indicated by "." (the current directory).
      let classpath = takeDirectory f ++ [classpathSep, '.']
      -- let classpath = ['.', classpathSep] ++ takeDirectory f
      (javaRet, javaOut, javaErr) <- readProcessWithExitCode "java" ["-cp", classpath, takeBaseName f] input
      if isExitFailure javaRet then do
        reportError "java" "non-zero exit code" (Just f) (nullMaybe input) (nullMaybe javaOut) (nullMaybe javaErr)
        return False
      else do
        -- Try to work around line ending problem
        let removeCR = filter (/= '\r')
        if trim (removeCR javaOut) == trim (removeCR output) then
          return True
        else do
          reportError "java" "invalid output" (Just f) (nullMaybe input) (nullMaybe javaOut) (nullMaybe javaErr)
          putStrLn "Expected output:"
          putStrLn $ color blue $ output
          return False
    else do
      let
        msg = concat
          [ "did not find any Java class file at \""
          , expectedJavaClassFilePath
          , "\" (note that the output Java class file must be written to same directory as the input C++ file)"
          ]
      reportError prog msg (Just f) (nullMaybe input) (nullMaybe compilerOut) (nullMaybe compilerErr)
      return False

listCCFiles :: FilePath -> M [FilePath]
listCCFiles dir = do
  doubles <- asks doublesFlag
  sort . filter (doublesFilter doubles) . filter ((==".cc") . takeExtension) <$> do
    liftIO $ listDirectoryRecursive dir
  where
    doublesFilter doubles filename =
      doubles || not (isInfixOf "double" filename || isInfixOf "subtyping" filename)

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive dir = do
  doesDirectoryExist dir >>= \case
    False -> return []
    True  -> do
      fs <- map (dir </>) <$> listDirectory dir
      concat . (fs:) <$> mapM listDirectoryRecursive fs

---------------------------------------------------------------------------
-- * Debugging
---------------------------------------------------------------------------

-- | Print debug message if debugging is on.
debug :: String -> M ()
debug = whenM (asks debugFlag) . liftIO . putStrLn

---------------------------------------------------------------------------
-- * Run programs
---------------------------------------------------------------------------

isExitFailure :: ExitCode -> Bool
isExitFailure ExitSuccess = False
isExitFailure ExitFailure{} = True

runPrgNoFail_ ::
     FilePath  -- ^ Executable
  -> [String]  -- ^ Flags
  -> FilePath  -- ^ Filename
  -> M ()
runPrgNoFail_ exe flags file = void $ runPrgNoFail exe flags file

runPrgNoFail ::
     FilePath            -- ^ Executable
  -> [String]            -- ^ Flag
  -> FilePath            -- ^ Filename
  -> M (String, String)  -- ^ stdout and stderr
runPrgNoFail exe flags file = do
  let args = flags ++ [file]
  liftIO $ hPutStr stderr $ "Running " ++ showCommandForUser exe args ++ "... "
  (s, out, err) <- liftIO $ readProcessWithExitCode exe args ""
  liftIO $ hPutStrLnExitCode s stderr "."
  case s of
    ExitFailure x -> liftIO do
      reportError exe ("with status " ++ show x) (Just file) Nothing (nullMaybe out) (nullMaybe err)
      exitFailure
    ExitSuccess -> do
      debug $ "Standard output:\n" ++ out
      debug $ "Standard error:\n" ++ err
      return (out,err)

---------------------------------------------------------------------------
-- * Terminal output colors
---------------------------------------------------------------------------

type Color = Int

color :: Color -> String -> String
#if defined(mingw32_HOST_OS)
color _ s = s
#else
color c s
  | haveColors = fgcol c ++ s ++ normal
  | otherwise  = s
#endif

-- | Colors are disabled if the terminal does not support them.
{-# NOINLINE haveColors #-}
haveColors :: Bool
haveColors = unsafePerformIO supportsPretty

highlight, bold, underline, normal :: String
highlight = "\ESC[7m"
bold      = "\ESC[1m"
underline = "\ESC[4m"
normal    = "\ESC[0m"

fgcol, bgcol :: Color -> String
fgcol col = "\ESC[0" ++ show (30+col) ++ "m"
bgcol col = "\ESC[0" ++ show (40+col) ++ "m"

red, green, blue, black :: Color
black = 0
red   = 1
green = 2
blue  = 6

-- Inlined from https://hackage.haskell.org/package/pretty-terminal-0.1.0.0/docs/src/System-Console-Pretty.html#supportsPretty :

-- | Whether or not the current terminal supports pretty-terminal
supportsPretty :: IO Bool
supportsPretty =
  hSupportsANSI stdout
  where
    -- | Use heuristics to determine whether the functions defined in this
    -- package will work with a given handle.
    --
    -- The current implementation checks that the handle is a terminal, and
    -- that the @TERM@ environment variable doesn't say @dumb@ (whcih is what
    -- Emacs sets for its own terminal).
    hSupportsANSI :: Handle -> IO Bool
    -- Borrowed from an HSpec patch by Simon Hengel
    -- (https://github.com/hspec/hspec/commit/d932f03317e0e2bd08c85b23903fb8616ae642bd)
    hSupportsANSI h = (&&) <$> hIsTerminalDevice h <*> (not <$> isDumb)
      where
        isDumb = (== Just "dumb") <$> lookupEnv "TERM"

---------------------------------------------------------------------------
-- * Checking files and directories
---------------------------------------------------------------------------

checkFileExists :: FilePath -> IO ()
checkFileExists f = do
  e <- doesFileExist f
  unless e do
    putStrLn $ color red $ quote f ++ " is not an existing file."
    exitFailure

checkDirectoryExists :: FilePath -> IO ()
checkDirectoryExists f = do
  e <- doesDirectoryExist f
  unless e do
    putStrLn $ color red $ quote f ++ " is not an existing directory."
    exitFailure

---------------------------------------------------------------------------
-- * Error reporting and output checking
---------------------------------------------------------------------------

colorExitCode :: ExitCode -> String -> String
colorExitCode ExitSuccess     = color green
colorExitCode (ExitFailure _) = color red

putStrLnExitCode :: ExitCode -> String -> IO ()
putStrLnExitCode e = putStrLn . colorExitCode e

hPutStrLnExitCode :: ExitCode -> Handle -> String -> IO ()
hPutStrLnExitCode e h = hPutStrLn h . colorExitCode e

reportErrorColor ::
     Color
  -> String         -- ^ command that failed
  -> String         -- ^ how it failed
  -> Maybe FilePath -- ^ source file
  -> Maybe String   -- ^ given input
  -> Maybe String   -- ^ stdout output
  -> Maybe String   -- ^ stderr output
  -> IO ()
reportErrorColor col c m f i o e = do
    putStrLn $ color col $ c ++ " failed: " ++ m
    whenJust f prFile
    whenJust i \ i -> do
      putStrLn "Given this input:"
      putStrLn $ color blue $ replaceNull i "<nothing>"
    whenJust o \ o -> do
      putStrLn "It printed this to standard output:"
      putStrLn $ color blue $ replaceNull o "<nothing>"
    whenJust e \ e -> do
      putStrLn "It printed this to standard error:"
      putStrLn $ color blue $ replaceNull e "<nothing>"

reportError ::
     String         -- ^ command that failed
  -> String         -- ^ how it failed
  -> Maybe FilePath -- ^ source file
  -> Maybe String   -- ^ given input
  -> Maybe String   -- ^ stdout output
  -> Maybe String   -- ^ stderr output
  -> IO ()
reportError = reportErrorColor red

prFile :: FilePath -> IO ()
prFile f = do
  whenM (doesFileExist f) do
    putStrLn $ "For input file " ++ f ++ ":"
    putStrLn $ "---------------- begin " ++ f ++ " ------------------"
    s <- readFile f
    putStrLn $ color green s
    putStrLn $ "----------------- end " ++ f ++ " -------------------"

-- | Report how many tests passed and which tests failed (if any).
--   Returns 'True' if all passed.
report :: String -> [(String, Bool)] -> IO Bool
report n rs = do
  let (passed, failed) = partition snd rs
  let ok = null failed
  let c  = if ok then green else red
  putStrLn $ color c $ n ++ "passed " ++ show (length passed) ++ " of " ++ show (length rs) ++ " tests"
  unless ok $
    mapM_ (putStrLn . color red) $ "Failed tests:" : map fst failed
  return ok

---------------------------------------------------------------------------
-- * Utilities for files
---------------------------------------------------------------------------

cleanDirectory :: FilePath -> [String] -> IO ()
cleanDirectory path exts = do
  files <- listDirectory path
  forM_ files \ f -> do
    when (takeExtension f `elem` exts) $
      cleanFile $ path </> f

cleanFile :: FilePath -> IO ()
cleanFile file = whenM (doesFileExist file) $ removeFile file

cleanFiles :: [FilePath] -> IO ()
cleanFiles = mapM_ cleanFile

quote :: FilePath -> FilePath
quote p = "'" ++ concatMap f p ++ "'"
  where
    f '\'' = "\\'"
    f c = [c]

readFileIfExists :: FilePath -> IO String
readFileIfExists f = catch (readFile f) \ (_ :: IOException) -> return ""

---------------------------------------------------------------------------
-- * General utilities for monads, lists, and strings
---------------------------------------------------------------------------

whenM :: Monad m => m Bool -> m () -> m ()
whenM mb m = mb >>= \b -> when b m

whenJust :: Applicative m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) k = k a
whenJust Nothing  _ = pure ()

ifNull :: [a] -> b -> ([a] -> b) -> b
ifNull [] b _ = b
ifNull as _ f = f as

replaceNull :: [a] -> [a] -> [a]
replaceNull as xs = ifNull as xs id

nullMaybe :: [a] -> Maybe [a]
nullMaybe as = ifNull as Nothing Just

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
