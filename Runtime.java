import java.lang.String;
import java.util.Locale;
import java.util.Scanner;

public class Runtime {

    private static Scanner scan = (new Scanner(System.in)).useLocale(Locale.ROOT);

    public static void printInt (int n) {
        System.out.println(n);
    }

    // Use the ROOT locale to force a decimal dot rather than the country-specific decimal separator.
    public static void printDouble (double x) {
        // The following attempt fixes the precision to 5 digits
        // and thus differs from a vanilla println(x).
        //
        // System.out.println(String.format(Locale.ROOT, "%f", x));
        //
        // I did not find an way to reproduce exactly the behavior of println(x)
        // using String.format.  Someone may find one, but I ran out of steam.
        // (E.g. I was suprised to learn that the code of Double.toString(),
        // invoked by println(double), does not invoke a formatter like String.format
        // but is instead a very ugly custom code: getBinaryToASCIIConverter(double).)
        //
        // Thus, for now I resort to the hacky solution to just globally set the locale.
        Locale.setDefault(Locale.ROOT);
        System.out.println(x);
    }

    public static int readInt () {
        return scan.nextInt();
    }

    public static double readDouble () {
        return scan.nextDouble();
    }

}
