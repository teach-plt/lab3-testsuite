PLT Lab 3 Test Suite
====================

Programming Language Technology (PLT, Chalmers DAT151, University of Gothenburg DIT231)

This is the test suite for PLT lab 3: Code generation for C--.

Prerequisites
-------------

The following tools need to be in the `PATH`:

- [Haskell Stack](https://docs.haskellstack.org/en/stable/) in a recent enough version, e.g. version 3.1.1.
- The [make](https://en.wikipedia.org/wiki/Make_(software)) tool.
- The [java](https://en.wikipedia.org/wiki/Java_(programming_language)) virtual machine.

Your solution directory needs to contain a `Makefile` with instructions
so that the invocation of `make` there builds your solution
and places it as executable `lab3` there.

Running the testsuite
---------------------

Invoke the test runner with the path to the directory containing your solution.
```
stack run -- path/to/solution/directory
```

This will first call `make` in this directory and then invoke the generated `lab3` there on all the test files.
It expects `lab3 path/to/file.cc` to generate a Java class file in the same directory, i.e. `path/to/file.class`.
It will run the generated class files and check whether their output is correct.

Getting the testsuite
---------------------

This testsuite is originally hosted at https://github.com/teach-plt/lab3-testsuite .
It contains https://github.com/teach-plt/lab2-testsuite as a git submodule,
so it needs to cloned recursively, e.g.:
```
git clone --recursive https://github.com/teach-plt/lab3-testsuite
```
