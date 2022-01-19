/*
 * Simple.java: a couple of simple Prolog-Java examples
 *
 * Note:This code is very old and does not reflect current best practices in Java.
 */

// Not all platforms use Latin 1 as default. Always use only the 7-bit ASCII range.
// 0xC5 == capital A ring, 0xC4 == capital A diaeresis, 0xD6 == capital O diaeresis
// 0xE5 == small a ring, 0xE4 == small a diaeresis, 0xF6 == small o diaeresis,

import java.util.HashMap;

import se.sics.jasper.SICStus;
import se.sics.jasper.SPTerm;
import se.sics.jasper.Term;

public class Simple {
    private String a;

    public Simple(String a, int b) {
        this.a = a + " " + b;
    }

    public static int square(int x) {
        return x * x;
    }

    public String get() {
        return a;
    }

    public void set(String b) {
        a = b;
    }

    public void append(String b) {
        a = new String(a + b);
    }

    public static SPTerm train(String filename) {
        SICStus sp;

        SPTerm way = null;
        HashMap<String, Term> varMap = new HashMap<>();

        try {
            @SuppressWarnings("deprecation")
            SICStus initializedSICStus = SICStus.getInitializedSICStus();
            if (initializedSICStus != null) {
                sp = initializedSICStus;
            } else {
                sp = new SICStus();
            }

            if (filename != null) {
                // You probably want to use sp.restore() instead
                sp.load(filename);
            }

            if (!sp.query("connected('Stockholm','\u00D6rebro',Way,Way).", varMap)) {
                System.out.println("ERROR: connected/4 failed");
            } else {
                way = ((SPTerm) varMap.get("Way"));
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return way;
    }

    // train when called from prolog and thus the prolog code is already loaded.
    public static SPTerm train() {
        return train(null);
    }

    public static void main(String argv[]) {
        SPTerm result = train("simple");
        if (result != null) {
            System.out.println(result.toString());
        } else {
            System.out.println("ERROR: Did not find any solutions"); // [PM] 3.10.1
        }
    }
}
