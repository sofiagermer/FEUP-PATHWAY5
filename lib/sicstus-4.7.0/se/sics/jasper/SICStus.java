/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

import java.io.File;
import java.io.IOException;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.net.JarURLConnection;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.Set;
import java.util.Stack;

/**
 * This class encapsulates the SICStus emulator and provides the basic
 * functionality for interacting with Prolog. The User's Manual documents in
 * more detail how to use this class. Refer to that document for more
 * information.
 *
 * <p>
 * This class uses <code>native</code> methods to interface with the C-Prolog
 * interface of SICStus Prolog. Hence, this class cannot be used from within an
 * applet.
 *
 * <p>
 * You should not inherit from <code>SICStus</code>.
 *
 */
public class SICStus implements Prolog {
    // Native methods. These interface directly to SICStus' C-interface.

    // [PM] 3.9 from SPException
    native String spErrorMessage(int errno);

    native int spGetErrno();

    // [PM] 3.9 from SPCanonicalAtom
    native int spRegisterAtom(long cAtom);

    native int spUnRegisterAtom(long cAtom);

    native long spAtomFromString(String string);

    native String spStringFromAtom(long cAtom);

    // [PM] 3.9 from SPQuery
    native int spNextSolution(long ref);

    native void spCutQuery(long ref);

    native void spCloseQuery(long ref);

    // [PM] 3.9 From SPTerm:
    native String spGetListChars(long term) throws NativeCodeException;

    native String spGetNumberChars(long term) throws NativeCodeException;

    native String spGetString(long term) throws NativeCodeException;

    native String spGetStringFromAtom(long canonical);

    native boolean spIsAtomic(long term);

    native boolean spIsList(long term);

    native boolean spIsNumber(long term);

    native boolean spUnify(long term1, long term2);

    native double spGetFloat(long termref) throws NativeCodeException;

    native Object spGetObject(long termref) throws NativeCodeException;

    native int spCompare(long term1, long term2);

    native void spConsFunctor(long term, long atm, long args[]) throws NativeCodeException;

    native void spConsList(long termref, long head, long tail) throws NativeCodeException;

    native int spGetFunctorArity(long term) throws NativeCodeException;

    native void spPutFloat(long termref, double d) throws NativeCodeException;

    native void spPutFunctor(long termref, long atm, int arity) throws NativeCodeException;

    native void spPutInteger(long termref, long i) throws NativeCodeException;

    native void spPutList(long termref) throws NativeCodeException;

    native void spPutListChars(long termref, String string) throws NativeCodeException;

    native void spPutNumberChars(long termref, String string) throws NativeCodeException;

    native void spPutString(long termref, String value) throws NativeCodeException;

    native void spPutVariable(long termref) throws NativeCodeException;

    native int spTermType(long term);

    native long spCreateGlobalRef(Object obj);

    native void spGetArg(long i, long term, long arg) throws NativeCodeException;

    native long spGetAtom(long term) throws NativeCodeException;

    native long spGetAtomFromString(String string);

    native long spGetFunctorCanonical(long term) throws NativeCodeException;

    native long spGetInteger(long termref) throws NativeCodeException;

    native void spGetList(long term, long head, long tail) throws NativeCodeException;

    native void spPutAtom(long termref, long canonical) throws NativeCodeException;

    native void spPutTerm(long term1, long term2) throws NativeCodeException;

    native String spPrintVariable(long termref);

    // private native int spInitialize031101(String argv[], String bootPath);
    // private native int spInitialize040003(String argv[]);
    private native int spInitialize040102(String argv[], String keys[], String values[]);

    private native void spFinalize();

    private native int spLoad(String qlFile);

    private native int spRestore(String savFile);

    // [PD] 3.9 see `SICStus.restore(String savFile)'
    private native int spReloadJasper();

    private native long spCall(String module, String name, long arg_refs[]);

    private native long spCallCutFail(String module, String name, long arg_refs[]);

    private native long spOpenCall(String module, String name, long arg_refs[]);

    private native long spOpenContext();

    private native int spExceptionIndex(int nextIndex);

    private native boolean spRetractException(int index);

    private native boolean spExceptionString(int index, long streamCode, int maxPrintDepth);

    private native long spMakeTermRef();

    private native void spResetTermRefs(long termref);

    private native long spGetTermRefs();

    private native boolean spValidTermRef(long termref);

    // [PM] 3.9b5 *only* call from openStringStream
    native long spOpenStringStream(String string);

    // [PM] 3.9b5 *only* call from closeStringStream
    native boolean spCloseStringStream(long streamCode);

    native long spOpenStringBufferStream(StringBuffer strBuf);

    private native boolean spIsEmptyList(long termref);

    // [PD] 3.9 Used by class Jasper.
    native boolean spSetThreadServerMode(boolean on);

    private static native String spNativeVersion();

    private static final long NO_API = 0;
    // 3.9 MULTI_SP api pointer
    private long iAPI = NO_API;

    /**
     * This is the path where SICStus finds the Runtime Library. The field is
     * instantiated when creating the SICStus object. This variable should not be
     * modified by the user or undefined results will occur.
     */

    boolean initialized = false;

    // Specify default values for these here.
    int debugLevelValue = 0;

    static int debugLevelValueOrig = 0;

    // Protected by 'this'
    boolean shouldCheckAgeFlag = true;

    boolean reuseTermRefsFlag = true;

    /**
     * Currently <strong>Unsupported</strong>; see the release notes for details.
     *
     * @return Undocumented
     *
     * @see SICStus#setShouldCheckAge
     */
    public final boolean shouldCheckAge() {
        synchronized (this) {
            return shouldCheckAgeFlag;
        }
    }

    /**
     * Currently <strong>Unsupported</strong>; see the release notes for details. If
     * the argument is <code>true</code> then the methods on {@link SPTerm SPTerm}
     * will throw exceptions when used in a way that may potentially create dangling
     * pointers in the Prolog heap. Returns the old value of the flag.
     *
     * <p>
     * Off by default in the interest of backward compatibility since it will throw
     * exception for some legal code. It is probably a good idea to turn this on for
     * new code (and for existing non-production code). This flag will probably be
     * on by default in the future, please try it and report good or bad experiences.
     * <p>
     * Can be set with the <code>SICStus.checkSPTermAge</code> system property.
     *
     * @param newVal
     *            Undocumented
     * @return the previous value
     */
    public final boolean setShouldCheckAge(boolean newVal) {
        synchronized (this) {
            boolean old = shouldCheckAgeFlag;
            shouldCheckAgeFlag = newVal;
            return old;
        }
    }

    /**
     * <strong>Unsupported</strong>.
     *
     * @return Undocumented
     *
     * @see SICStus#setReuseTermRefs
     */
    public final boolean reuseTermRefs() {
        return reuseTermRefsFlag;
    }


    /**
     * <strong>Unsupported</strong>; see the release notes for details. If the
     * argument is <code>false</code> then {@link SPTerm#delete SPTerm.delete()}
     * will <strong>not</strong> make the SP_term_ref associated with the deleted
     * <code>SPTerm</code> object available for re-use. Returns the old value of the
     * flag.
     * <p>
     * This flag is currently <strong>on</strong> by default, there should be no
     * reason to turn it off.
     *
     * Can be set with the <code>SICStus.reuseTermRefs</code> system property.
     *
     * @param newVal
     *            Undocumented
     * @return the previous value
     */
    public final boolean setReuseTermRefs(boolean newVal) {
        synchronized (this) {
            return setReuseTermRefsFlag(newVal);
        }
    }

    // must be called while synchronized on sp
    boolean setReuseTermRefsFlag(boolean newVal) {
        boolean old = reuseTermRefsFlag;
        reuseTermRefsFlag = newVal;
        return old;
    }

    /**
     * Currently <strong>Unsupported</strong>; see the Release Notes. This method
     * may disappear in future versions Get the current debug level.
     *
     * @return Undocumented
     *
     * @see SICStus#setDebugLevel
     */
    public final int debugLevel() {
        return debugLevelValue;
    }

    final boolean debugging(int minLevel) {
        return debugLevelValue >= minLevel;
    }

    final boolean debugging() {
        return debugging(1);
    }

    static void dbgPrintln(String msg) {
        System.err.println("% DBG [Thread " + Thread.currentThread().getName() + "]: " + msg);
        System.err.flush();
    }

    /**
     * Currently <strong>Unsupported</strong>. This method may disappear in future
     * versions. Enable run-time debug checks. A value of zero disables debug
     * checks. Returns the old debugging level. The initial debug level can be set
     * with the <code>SICStus.debugLevel</code> system propery.
     *
     * @param level
     *            Undocumented
     * @return the previous value
     */
    public int setDebugLevel(int level) {
        synchronized (this) {
            int old = debugLevelValue;
            debugLevelValue = level;

            if (old >= 1) {
                if (old != level) {
                    dbgPrintln("Changing debug level from " + old + " to " + level);
                }
            }
            return old;
        }
    }

    /**
     * [PM] 3.8.5 Top of the linked list of all live SPTerms. Each new SPTerm is
     * pushed here. SPQuery close/cut/nextsolution/return to Prolog pops and
     * invalidates from this stack to keep in synch with the deallocation done of
     * term references on the Prolog side. Do not touch!
     **/
    SPTerm term_stack;
    /**
     * [PM] 3.8.5 Top of the linked list of all open SPQuery-ies Each new SPQuery is
     * pushed here. SPQuery close (and perhaps cut, nextsolution, close on older
     * query) pops and invalidates from this stack to keep in synch with the Prolog
     * side. Do not touch!
     **/
    SPQuery query_stack;

    void term_stack_unlink(SPTerm t, SPQuery start) throws SPException {
        SPTerm prev = null;
        SPTerm stack;
        SPTerm x;

        // find pred such that prev.next == t;

        if (start == null) {
            stack = term_stack;
        } else {
            stack = start.term_stack_mark;
        }

        for (prev = null, x = stack; x != t; x = x.next) {
            if (x == null) {
                // invariant violation
                String msg = "** ERROR: Internal Jasper Error: could not find term to unlink";
                if (debugging())
                    SICStus.dbgPrintln(msg);
                throw new SPException(this, 0, msg);
            }
            prev = x;
        }

        // here prev.next == t
        // t can be referenced from (at most) three kinds of places
        // . prev.next (the term stack)
        // . start.term_stack_mark, start.next.term_stack_mark, ... a
        // consecutive sequence
        // . sp.term_stack
        if (prev != null) {
            prev.next = t.next;
        }
        {
            SPQuery tmpQuery;
            if (debugging())
                SICStus.dbgPrintln("Unlinking " + t.superToString() + "nativeTermRef=" + t.nativeTermRef);
            for (tmpQuery = start; tmpQuery != null && tmpQuery.term_stack_mark == t; tmpQuery = tmpQuery.next) {
                if (debugging())
                    SICStus.dbgPrintln("  From " + tmpQuery.superToString());
                tmpQuery.term_stack_mark = t.next;
            }
        }
        if (term_stack == t) {
            if (debugging())
                SICStus.dbgPrintln("Unlinking from term_stack " + t.superToString());
            term_stack = t.next;
        }
    }

    // depth of choice point stack.
    int age() {
        // the age of the empty term stack is zero
        return (query_stack == null ? 0 : query_stack.age());
    }

    /**
     * This main-function is just a small test function. It will try to load SICStus
     * by creating a SICStus object and prints a message if it succeeded.
     *
     * @param argv
     *            the command line arguments
     *
     * @see SICStus#SICStus()
     */
    static public void main(String argv[]) {
        try {
            // Do not remove this message
            System.out.println("Trying to load SICStus.");
            SICStus sp = new SICStus();
            // [PM] 4.2 Let SICStus write the message for increased
            // confidence that everything works. Also, it makes for a
            // tiny demo of DSRT, .e.g. env SP_USE_DEVSYS=yes java -jar jasper.jar

            // Do not remove this message
            sp.query("write('If you see this message, you have successfully'),nl,"
                    + "write('initialized the SICStus Prolog engine.'),nl," + "flush_output.", null);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    void dummy() {
        System.out.println("Enter " + this + ".dummy()");
        System.out.println(this + ".getSPAPI() == " + getSPAPI());
        System.out.println("Exit " + this + ".dummy()");
    }

    // [PM] 3.9 only called by spnative.c
    long getSPAPI() {
        return iAPI;
    }

    // [PM] 3.9 only called by spnative.c
    int setSPAPI(long spapi) {
        if (iAPI != 0)
            return SP_GLUEFAILURE;
        iAPI = spapi;
        return SP_SUCCESS;
    }

    // // Called by jasper_yield to pass control over to Java.
    // static void YieldToJava() {
    // // Need not do anything, all functionality is in the glue.
    // }

    static boolean nativeCodeIsLoaded = false;

    /*
     * Load Jasper native code. To keep this simple, we let the OS take care of
     * searching by calling {@link java.lang.System#loadLibrary() loadLibrary()}.
     * [PM] 3.8.4 Still true? Usually, the jasper library (libjasper.dll or
     * libjasper.so) should be located in the same directory as the main SICStus
     * runtime kernel (sprt38.dll or libsprt38.dll).
     */
    // [PD] 3.11.1 This method is a no-op if called more than once.
    // void loadNativeCode(String bootPath) {
    static void loadNativeCode() {
        if (debugLevelValueOrig > 0)
            dbgPrintln("nativeCodeIsLoaded==" + nativeCodeIsLoaded);
        if (nativeCodeIsLoaded)
            return;
        String libName = "jasper";

        libName = "spnative"; // [PM] 3.9 NEW_WORLD_ORDER
        boolean find_spnative_heuristic = // [PD] 3.11.1
                !Boolean.getBoolean("NoSPNativeHeuristic");

        if (find_spnative_heuristic && System.getProperty("path.separator").equals(":")) { // unix

            // [PD] 3.11.1 eliminate the need for the user to specify
            // -Djava.library.path to the Java launcher.
            try {
                if (debugLevelValueOrig > 0)
                    dbgPrintln("Attempting System.loadLibrary(\"" + libName + "\")");
                // If the property java.library.path is set to something
                // sane, this should work
                System.loadLibrary(libName);
            } catch (@SuppressWarnings("unused") UnsatisfiedLinkError linkError) {
                // Try to find libspnative.so (or equivalent) in the SICStus installation tree
                URL classURL = SICStus.class.getResource("SICStus.class");
                if (debugLevelValueOrig > 0)
                    dbgPrintln("classURL==" + classURL);
                String realLibName = System.mapLibraryName(libName);
                try {
                    JarURLConnection juc = (JarURLConnection) classURL.openConnection();
                    String jarFileName = juc.getJarFile().getName();
                    if (debugLevelValueOrig > 0)
                        dbgPrintln("jarFileName==" + jarFileName);
                    File jarFile = new File(jarFileName);
                    // jasper.jar is
                    // <installpath>/lib/sicstus-<version>/bin/jasper.jar
                    // lib path is <installpath>/lib
                    // This does the trick:
                    File libPath = jarFile.getParentFile().getParentFile().getParentFile();
                    if (debugLevelValueOrig > 0)
                        dbgPrintln("libPath==" + libPath);
                    if (debugLevelValueOrig > 0)
                        dbgPrintln("realLibName==" + realLibName);
                    String fullPath = new File(libPath, realLibName).getPath();
                    if (debugLevelValueOrig > 0)
                        dbgPrintln("Attempting System.load(\"" + fullPath + "\")");
                    try {
                        // "UnsatisfiedLinkError - if the file does not exist."
                        System.load(fullPath);
                    } catch (UnsatisfiedLinkError ex) {
                        // [PM] 4.3 We now use .dylib for JNI extension.
                        // Pre Java 8 System.mapLibraryName() uses ".jnilib"
                        // Java 8 System.mapLibraryName() uses ".dylib"
                        // All versions (from Java 5) allows ".dylib" in
                        // System.loadLibrary() but we got here because
                        // System.loadLibrary did not find spnative JNI library
                        // in the "usual places".
                        String oldSuffix = ".jnilib";
                        String newSuffix = ".dylib";
                        if (fullPath.endsWith(oldSuffix)) {
                            // Assume OS X, Java pre-8, fall back to .dylib
                            String newPath = fullPath.substring(0, fullPath.length() - oldSuffix.length())
                                    .concat(newSuffix);
                            if (debugLevelValueOrig > 0)
                                dbgPrintln("Attempting fallback to System.load(\"" + newPath + "\")");
                            System.load(newPath);
                        } else {
                            // Not OS X, just rethrow
                            throw (ex);
                        }
                    }
                } catch (IOException ex) {
                    if (debugLevelValueOrig > 0)
                        System.err.println(ex);
                    throw new UnsatisfiedLinkError("Cannot locate " + realLibName);
                }
            }
        } else { // Windows or !find_spnative_heuristic
            if (debugLevelValueOrig > 0)
                dbgPrintln("Calling System.loadLibrary(\"" + libName + "\")");
            System.loadLibrary(libName);
            if (debugLevelValueOrig > 0)
                dbgPrintln("Called System.loadLibrary(\"" + libName + "\")");
        }
        // [PD] 3.11.1 Check for version mismatch
        String nativeVersion = SICStus.spNativeVersion();
        if (debugLevelValueOrig > 0) {
            dbgPrintln("nativeVersion==" + nativeVersion);
            dbgPrintln("Version.SICStusVersion==" + Version.SICStusVersion);
        }
        if (!nativeVersion.equals(Version.SICStusVersion)) {
            throw new UnsatisfiedLinkError("Version of Class SICStus (" + Version.SICStusVersion
                    + ") does not match version of spnative (" + nativeVersion + ")");
        }
        nativeCodeIsLoaded = true;
    }

    // Constants
    private static final int SP_SUCCESS = 1;
    private static final int SP_ERROR = -1;
    private static final int SP_FAILURE = 0;
    private static final int SP_GLUEFAILURE = -2;
    private static final int SP_JNIENV_SET = -3;

    private static final String SICSTUS_PROPERTY_PREFIX = "se.sics.sicstus.property.";

    // [PM] 3.9 remove for multi-SP (later) DO NOT USE IN JASPER ITSELF
    // Contains a reference to the most recently created instance of SICStus.
    // If we keep this we should somehow do something reasonable when
    // a SICStus object is deinitialized.
    //
    // // Contains the reference to the one and only instance of SICStus.
    // [PM] Replaced by spGlobalStack below.
    // static SICStus spGlobal;

    // [PM] 3.9 To support getInitializedSICStus() functionality when
    // there can be multiple SICStus object we use the following
    // (somewhat heuristic) approach:
    // A thread local stack of SICStus objects.
    //
    // * Each time prolog meta-calls a Java method the SICStus object
    // of the sprt is pushed on the stack and then popped when
    // returning to prolog.
    //
    // * Each time a SICStus object is created and the top-of-the
    // stack is null (or empty?) the top-element is set to the newly
    // created SICStus object.
    //
    // The stack should be associated with the Java thread (not the
    // (planned) SICStus server thread).
    //
    // Currently we implement this in MetaGlue_(Push/Pop)Context

    // No initialValue() method, because we want to distinguish empty stack from no
    // (thread-local) stack.
    private static final ThreadLocal<Stack<SICStus>> spGlobalStack = new ThreadLocal<>();

    // [PM] 3.9 for those threads that have no thread local concept of
    // "current SP" we use the last pushed from any thread. In
    // particular this will give the 3.8 behaviour when there is
    // only one SICStus instance.
    private static SICStus defaultSPGlobal = null;

    // [PM] 3.9 Now only allow calling from single thread again. Use
    // null to mean "do not check".
    //
    // Reference to the thread from which SICStus was initialized.
    // It is not allowed to call SICStus from any other thread, since
    // the C-Prolog interface is not reentrant.
    private final Thread iAttachedThread;

    void popToTermStackMark(SPTerm term_stack_mark) {
        synchronized (this) {
            // Do not attempt SPTerm toString here (e.g., for debug)
            // dbgPrintln("term_stack_mark==" + term_stack_mark.nativeTermRef);
            while (term_stack != term_stack_mark) {
                // dbgPrintln("sp.term_stack==" + sp.term_stack.nativeTermRef);
                // dbgPrintln("sp.term_stack.next==" +
                // sp.term_stack.nativeTermRef);
                SPTerm tmp = term_stack;
                term_stack = tmp.next;
                tmp.invalidate();
            }
        }
    }

    /**
     * Throws an exception if the current thread is not the thread which created
     * this instance of the SICStus object.
     *
     * @see SICStus#isLegalCaller
     */
    /* public (not public in 3.9) */
    void checkLegalCaller()
            // throws IllegalCallerException
            throws SPException {
        if (!isLegalCaller())
            throw new IllegalCallerException(this, iAttachedThread, Thread.currentThread());
    }

    /**
     * Similar to {@link #checkLegalCaller() checkLegalCaller()}, but returns false
     * instead of throwing an exception.
     *
     * @return true if the current thread is allowed to call methods in the
     *         SICStus-Prolog interface, false otherwise.
     */
    boolean isLegalCaller() {
        return iAttachedThread == Thread.currentThread();
    }

    // [PM] 3.9 retained for backward compatibility only (does not
    // make sense with multiple sicstus instances).
    // DO NOT USE IN JASPER ITSELF
    /**
     * <strong>This documentation is not up-to-date!</strong>. Returns a pointer to
     * the SICStus object which has been initialized in this JVM. Normally this
     * reference is obtained by calling new SICStus(), but if Java has been invoked
     * from within SICStus, it is not possible to call new SICStus() (since SICStus
     * has already been initialized). Instead, this method can be used.
     * <p>
     *
     * <strong>Deprecated</strong>, avoid use. This method tries to do something
     * sensible if there are multiple sicstus objects and will work as expected if
     * each SICStus is created in its own thread.
     *
     * @return A reference to an initialized SICStus object, or null if no SICStus
     *         object has been created.
     *
     * @deprecated do not use.
     */
    @Deprecated
    public static SICStus getInitializedSICStus() {
        // TODO: what about calling getInitializedSICStus() from a
        // thread other than the one that created the SICStus
        // object? In 3.8 that would return the (in 3.8: only)
        // SICStus object.
        SICStus sp = topSPGlobal();
        if (sp != null) {
            return sp;
        } else {
            return defaultSPGlobal;
        }
    }

    // 3.9 Create a SICStus object corresponding to an SP run-time.
    // Called from jasper.c when attaching to Java.
    static SICStus getNewSICStusFromAPIPtr(long api) throws SPException {
        return new SICStus(api);
    }

    private SICStus(String argv[], long spapi) throws SPException {
        SICStus sp = this;

        iAttachedThread = Thread.currentThread();

        // Not clear whether synchronizing on the class makes any sense, but this is
        // what was done prior to 4.5.
        synchronized (sp.getClass()) {
            int rval;

            // synchronizing on sp is (at least formally) needed
            // for shouldCheckAgeFlag et al.
            synchronized (sp) {
                sp.iAPI = spapi;

                debugLevelValueOrig = Integer
                        .getInteger("SICStus.debugLevel", Integer.getInteger("SICStus.debugLevelDefault", 0))
                        .intValue();
                sp.debugLevelValue = debugLevelValueOrig;

                if (sp.debugging()) {
                    dbgPrintln("sp.debugLevelValue == " + sp.debugLevelValue);
                }

                // Should we barf if putting new stuff into an old SPTerm?
                // Off by default since it is more restrictive than it need to
                // be and therefore a potential compatibility problem.
                sp.shouldCheckAgeFlag = ((System.getProperty("SICStus.checkSPTermAge") != null) ?
                // specified by user
                        Boolean.getBoolean("SICStus.checkSPTermAge") :
                        // default value
                        ((System.getProperty("SICStus.checkSPTermAgeDefault") != null)
                                ? Boolean.getBoolean("SICStus.checkSPTermAgeDefault") // default specified
                                :
                                // default is specified by initialization above
                                sp.shouldCheckAgeFlag));
                if (sp.debugging()) {
                    dbgPrintln("sp.shouldCheckAgeFlag == " + sp.shouldCheckAgeFlag);
                }

                // Should we maintain a free list of native SP_term_refs
                sp.reuseTermRefsFlag = ((System.getProperty("SICStus.reuseTermRefs") != null) ?
                // specified by user
                        Boolean.getBoolean("SICStus.reuseTermRefs") :
                        // default value
                        ((System.getProperty("SICStus.reuseTermRefsDefault") != null)
                                ? Boolean.getBoolean("SICStus.reuseTermRefsDefault") // default
                                                                                     // specified
                                :
                                // default is specified by initialization above
                                sp.reuseTermRefsFlag));

                if (sp.debugging()) {
                    dbgPrintln("sp.reuseTermRefsFlag == " + sp.reuseTermRefsFlag);
                }

                SICStus.loadNativeCode();

                // [PM] 3.9 If not already set push this (initial) SICStus.
                if (topSPGlobal() == null) {
                    sp.pushSPGlobal();
                }

                // // multi-SP (but retain for backward compatibility. A ref to the
                // *first* created sp)
                // if (SICStus.spGlobal == null) {
                // SICStus.spGlobal = sp;
                // }

                // done by spInitialize: sp.spSetSPGlobal();
                if (sp.debugging()) {
                    dbgPrintln("Maybe call sp.spInitialize...(" + argv + "), api==" + sp.iAPI);
                }

                if (sp.iAPI == 0) { // api != 0 if called from sicstus run-time
                    String spInitializeVER = "spInitialize040102";
                    Properties props;
                    Map<String, String> sicstusProps = new HashMap<>();

                    try {
                        props = System.getProperties();
                    } catch (@SuppressWarnings("unused") SecurityException e) {
                        // We were not allowed to read the properties.
                        props = null;
                    }
                    if (props != null) {
                        for (String key : props.stringPropertyNames()) {
                            if (key.startsWith(SICSTUS_PROPERTY_PREFIX)) {
                                String sicstusKey = key.substring(SICSTUS_PROPERTY_PREFIX.length());
                                if (sp.debugging())
                                    dbgPrintln("SICStus Property \"" + sicstusKey + "\"=\"" + props.getProperty(key)
                                            + "\"");
                                if (sicstusKey.length() > 0) {
                                    sicstusProps.put(sicstusKey, props.getProperty(key));
                                }
                            }
                        }
                    }
                    String[] keys = null;
                    String[] values = null;
                    if (sicstusProps.size() > 0) {
                        keys = new String[sicstusProps.size()];
                        values = new String[sicstusProps.size()];
                        int i = 0;
                        for (Entry<String, String> entry : sicstusProps.entrySet()) {
                            keys[i] = entry.getKey();
                            values[i] = entry.getValue();
                            i++;
                        }
                    }

                    if (sp.debugging())
                        dbgPrintln("Calling sp." + spInitializeVER + "(" + argv + ", " /* + sp.bootPath */ + "), api=="
                                + sp.iAPI);

                    rval = sp.spInitialize040102(argv, keys, values);
                    if (sp.debugging())
                        dbgPrintln("Called sp." + spInitializeVER + "(" + argv + ", " /* + sp.bootPath */ + ") == "
                                + rval + ", api==" + sp.iAPI);

                    if (rval == SP_ERROR || rval == SP_FAILURE) {
                        throw new SPException(sp, "Could not initialize SICStus run-time");
                    } else if (rval == SP_GLUEFAILURE) {
                        throw new SPException(sp, "Error in Java glue code");
                    } else if (rval == SP_JNIENV_SET) {
                        throw new SPException(sp, "SICStus/JNI has already been initialized 2");
                    }
                }

                // 3.8.6 SPTerm.delete() require that the initial query
                // refers to an SPTerm. InitialSPTerm() creates an SPTerm
                // object for this purpose. Fixes SPRM 2063.
                @SuppressWarnings("unused")
                SPTerm ignore = SPTerm.InitialSPTerm(sp);
                // ensure that query stack is never empty
                // This is required so there is always a place to put the free list
                // for SP_term_ref reuse
                sp.pushQueryMark();

                sp.initialized = true;
            }
        }
    }

    // Called from getNewSICStusFromAPIPtr() which see
    SICStus(long spapi) throws SPException {
        this(null, spapi);
    }

    /**
     * Creates a SICStus object. Equivalent to {@link #SICStus(String[],String)
     * SICStus(null,null)}.
     *
     * @throws SPException
     *             Something went wrong during startup.
     * @see SPException
     * @see SICStus#SICStus(String[],String)
     */
    public SICStus() throws SPException {
        this((String[]) null);
    }

    /**
     * Creates a SICStus object.
     *
     * @param bootPath
     *            Ignored
     * @throws SPException
     *             Something went wrong during startup.
     * @deprecated Use {@link #SICStus()} instead.
     */
    @Deprecated
    public SICStus(String bootPath) throws SPException {
        this();
    }

    /**
     * Creates a SICStus object. This corresponds roughly to the C-Prolog interface
     * function {@code SP_initialize(argc, argv, options)}. It initializes the
     * emulator's memory manager, allocates WAM memory areas, etc. It also loads the
     * SICStus Runtime Library ( {@code sprt.sav}), also known as the
     * <i>bootfile</i>. If the bootfile cannot be found, you may specify the exact
     * location of it by using the system property {@code sicstus.path}. If you need
     * to set {@code sicstus.path}, it should be set to the parent directory of the
     * directory where the Runtime Library is located.
     * <p>
     * Any (Java) system property {@code se.sics.sicstus.property.}<i>NAME</i> will
     * be passed to the created SICStus instance as the (Prolog) system property
     * <i>NAME</i>.
     *
     * @param argv
     *            Argument vector to the emulator.
     * @throws SPException
     *             Something went wrong during startup.
     * @see SICStus#getInitializedSICStus
     * @since SICStus 4.5.0
     *
     */
    public SICStus(String argv[]) throws SPException {
        this(argv, NO_API);
    }

    /**
     * @param argv
     *            Argument vector to the emulator.
     * @param bootPath
     *            Ignored.
     * @throws SPException
     *             Something went wrong during startup.
     * @deprecated Use, the equivalent, {@link #SICStus(String[])} instead.
     */
    @Deprecated
    public SICStus(String argv[], String bootPath) throws SPException {
        this(argv);
    }

    /**
     * Interface to <code>SP_load()</code>, which in turn calls the Prolog predicate
     * load_files/[1,2] (See the User's
     * Manual). <strong>Note:</strong> The preferred way to load Prolog code is
     * through {@link SICStus#restore restore}.
     *
     * @param file
     *            The Prolog code to load. This can be a .pl-file, or a .po-file.
     * @throws SPException
     *             A Prolog exception was thrown.
     * @see SICStus#restore
     */
    synchronized public void load(String file) throws SPException // ,
                                                                  // IllegalCallerException
    {
        // [PM] 3.8.5 checkLegalCaller();
        checkLegalCaller(); // 3.9
        handleQueryResult(spLoad(file));
    }

    /**
     * Interface to <code>SP_restore()</code>. Restores the specified .sav-file.
     * This is the preferred way of loading Prolog code instead of using
     * {@link SICStus#load load}. This method must be called <strong>before</strong>
     * any other SICStus method is called, and may only be used if the SICStus
     * object has been created with <code>new</code>. Note that calling this method
     * will unload all foreign resources.
     * <p>
     * Example:
     *
     * <pre>
     * % sicstus
     * SICStus 3.7 beta3: Tue Jun 02 12:29:02 MET DST 1998
     * | ?- compile(bench),save_program('bench.sav').
     * {compiling /home/jojo/sicstus/sicstus3/bench.pl...}
     * {/home/jojo/sicstus/sicstus3/bench.pl compiled, 90 msec 6384 bytes}
     * {SICStus state saved in /home/jojo/sicstus/sicstus3/bench.sav}
     *
     * yes
     * | ?- halt.
     * </pre>
     *
     * and then restore <code>bench.sav</code> from Java by calling
     * <code>SICStus.restore()</code>.
     *
     * <pre>
     * restore(&quot;bench.sav&quot;);
     * </pre>
     *
     * @param savFile
     *            The .sav-file to restore.
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SICStus#load
     */
    synchronized public void restore(String savFile) throws SPException // ,
                                                                        // IllegalCallerException
    {
        // [PM] 3.8.5 checkLegalCaller();
        checkLegalCaller(); // 3.9
        handleQueryResult(spRestore(savFile));
        /*
         * [PD] 3.9 library(jasper) is no longer loaded, so don't reload it. // [PD] 3.9
         * Reload library(jasper). This is neccessary since spRestore() // unloads
         * foreign resources (via SP_restore()) . // This should perhaps be done in a
         * different way, e.g. by // making SP_restore() not unload library(jasper).
         * handleQueryResult(spReloadJasper());
         */
    }

    // [PM] 3.8.5 Removed. Not thread safe, and finalizers are
    // useless in Java anyway. Should add a proper interface to
    // spFinalize instead
    // Was a time-bomb since all Prolog to Java calls created a
    // new SICStus object. When any of these were zapped by the
    // GC SICStus would be deinitialized...
    /**
     * Deallocates memory used by native code and calls <code>SP_deinitialize</code>
     * to deallocate and unload resources used by the emulator.
     */
    /*
     * public void finalize() { this.spGlobal = null; spFinalize(); }
     */

    long[] termArrayToLongArray(SPTerm args[]) throws IllegalTermException {
        int i;
        long arg_refs[];

        arg_refs = new long[args.length];

        // Convert an SPTerm[] into a long[] containing term references
        for (i = 0; i < args.length; i++) {
            args[i].CheckValid(this); // [PM] 3.8.5 Perhaps wrap in
                                      // getNativeTermRef()
            arg_refs[i] = args[i].nativeTermRef;
        }

        return arg_refs;
    }

    // Called by SPTerm contructor
    // must be called while synchronized on sp object
    long makeTermRef() {
        long termRef = SPTerm.SP_ILLEGAL_TERM_REF;

        if (reuseTermRefs() && query_stack != null) {
            termRef = query_stack.getTermRef();
        }

        if (termRef == SPTerm.SP_ILLEGAL_TERM_REF) {
            termRef = spMakeTermRef();
        } else {
            // Detect if any problems (not expected but [PM] do not want this
            // feature to blow up in his face.)
            if (!spValidTermRef(termRef)) {
                // turn the broken feature off. (not fail proof, may be inside
                // readfromstring that binds it to true.)
                setReuseTermRefs(false);
                termRef = spMakeTermRef(); // not needed, given the exception
                                           // thrown below
                throw new NullPointerException("** ERROR: Internal Jasper Error: reused SP_term_ref not valid");
            }
        }

        return termRef;
    }

    /**
     * Reusable exception indices.
     * <p>
     * Exception propagation.
     * <p>
     * Thrown exception terms are maintained (asserted) in Prolog, keyed by an
     * integer index.
     * <p>
     * The strategy is to assert the exception term (this is done in spnative.c, in
     * the procedure `spExceptionIndex') with an index to be able to retrieve it
     * later. The index is stored in a int attribute in the class SPException.
     * <p>
     * When a {@link SPException} (with an asserted index) is created, a weak
     * reference to it is associated with the index, in
     * {@link #iExceptionIndexRegistry}. Later, when the {@link SPException} object
     * becomes garbage, the weak reference to it will appear on
     * {@link #iExceptionIndexRegistryQueue}. The indices used by these unused
     * exceptions are reclaimed by calling back to Prolog to retract the
     * corresponding asserted exception term.
     * <p>
     * The no longer used index is pushed on {@link #iExceptionIndexStack} and
     * re-used. This will ensure that the number of asserted exception terms, and
     * the size of the integer indices, will not grow unchecked.
     */
    private final Stack<Integer> iExceptionIndexStack = new Stack<>();

    /**
     * Bumped when we need a new index. Index 0 is not used for asserting an
     * exception term. We use the return value 0 for error return.
     */
    private int iAssertIndex = 0;

    // Assertion-index ==> WeakReference<SPExcption>
    private final Map<Integer, IndexReference> iExceptionIndexRegistry = new HashMap<>();
    private final ReferenceQueue<SPException> iExceptionIndexRegistryQueue = new ReferenceQueue<>();

    private static class IndexReference extends WeakReference<SPException> {
        // We need a separate field for the index, because the exception object in
        // this.get() will have been cleared by the GC, so we can not extract the index
        // from it.
        private final int iAssertIndex;

        public IndexReference(SPException e, ReferenceQueue<? super SPException> q) {
            super(e, q);
            iAssertIndex = e.getAssertIndex();
        }

        public int getAssertIndex() {
            return iAssertIndex;
        }
    }

    private int nextAssertIndex() {
        if (iExceptionIndexStack.isEmpty()) {
            iAssertIndex += 1;
            return iAssertIndex;
        } else {
            return iExceptionIndexStack.pop().intValue();
        }
    }

    /**
     * Ensure that, when {@code e} is garbage collected, the asserted exception term
     * is eventually retracted.
     *
     * @param e
     *            A {@link SPException} with a non-zero
     *            {@link SPException#getAssertIndex() assertion index}.
     */
    private void registerExceptionIndex(SPException e) {
        iExceptionIndexRegistry.put(Integer.valueOf(e.getAssertIndex()),
                new IndexReference(e, iExceptionIndexRegistryQueue));
        // Since we are the only thread that polls the reference queue, we do _not_
        // need a Reference.reachabilityFence(e) here.
    }

    /**
     * Returns the index for the current exception term.
     * <p>
     * Will perform cleanup actions. This includes calling back to Prolog if there
     * are old exception indices that can be reclaimed.
     *
     * @return the assertion index. Returns 0 if there was a problem.
     */
    private int spgetExceptionIndex() {
        synchronized (this) {
            int nextIndex = nextAssertIndex();
            /*
             * [PM] 4.3 SP_exception_term() must be called "immediately" in order to be sure
             * that the exception is still available.
             */
            int index = spExceptionIndex(nextIndex);

            if (index == 0) {
                // Reuse the index. It has no corresponding asserted term, so
                // spRetractException() should not be called.
                iExceptionIndexStack.push(Integer.valueOf(nextIndex));
            }

            if (debugging(1)) {
                dbgPrintln("In spgetExceptionIndex() (nextIndex=" + nextIndex + " (this is expected)");
            }

            // Reclaim any indexes that have become unreachable since last time.
            // Unfortunately, this can not be done before calling nextAssertIndex(), since
            // we must call spExceptionIndex() _immediately_ before calling back to Prolog.
            reclaimExceptionIndices();

            return index;
        }
    }

    /**
     * Reclaim any indexes that have become unreachable since last time.
     * <p>
     * For each index that has become unreachable, call back to Prolog to retract
     * its corresponding exception term.
     */
    private void reclaimExceptionIndices() {
        IndexReference ref;
        while ((ref = (IndexReference) iExceptionIndexRegistryQueue.poll()) != null) {
            int assertionIndex = ref.getAssertIndex();
            Integer boxedIndex = Integer.valueOf(assertionIndex);

            if (debugging(1)) {
                dbgPrintln("Found exception on reference queue. Reclaiming index " + assertionIndex
                        + " (this is expected)");
            }

            iExceptionIndexRegistry.remove(boxedIndex);

            // Call back to Prolog to retract the exception term
            if (spRetractException(assertionIndex)) {
                iExceptionIndexStack.push(boxedIndex);
            }
        }
    }

    private int exceptionTermPrintDepth = Integer.getInteger("SICStus.printDepth", 5).intValue();

    /**
     * Currently <strong>Unsupported</strong>. This method may disappear in future
     * versions. Set the maximum print depth for the string representation of
     * exception terms (this is what the {@link SPException#toString() toString}
     * method of the class {@link SPException SPException} returns).
     *
     * @param level
     *            The new maximum print depth for exception terms.
     * @return The old maximum print depth value. The initial maximum print depth
     *         can be set with the <code>SICStus.printDepth</code> system property.
     *         The default value is 5.
     */
    public int setPrintDepth(int level) {
        synchronized (this) {
            int old = exceptionTermPrintDepth;
            exceptionTermPrintDepth = level;
            return old;
        }
    }

    boolean handleQueryResult(long result) throws SPException {
        if (debugging(2))
            dbgPrintln("handleQueryResult() called with result = " + result);

        if (result == SP_SUCCESS)
            return true;
        if (result == SP_FAILURE)
            return false;
        if (result == SP_ERROR) {
            int speIndex;
            SPException e;

            if (debugging(2)) {
                dbgPrintln("SP_ERROR in handleQueryResult().\n");
            }

            speIndex = spgetExceptionIndex();

            if (debugging(2))
                dbgPrintln("SP_ERROR: `" + speIndex + "'\n");

            if (speIndex != 0) {
                StringBuffer sb = new StringBuffer();
                boolean spExceptionStringSuccess = false;
                long streamCode = 0;
                try {
                    streamCode = openStringBufferStream(sb);
                    spExceptionStringSuccess = spExceptionString(speIndex, streamCode, exceptionTermPrintDepth);
                } finally {
                    // [PM] 4.0.2+ close before trying to extract string from sb!
                    closeStringStream(streamCode);
                }
                String termString = (spExceptionStringSuccess ? sb.toString() : null);
                e = new SPException(this, termString, speIndex);
                registerExceptionIndex(e);

                if (debugging())
                    dbgPrintln("handleQueryResult() created e (speIndex!=0).\n");
                throw e;
            } else {
                if (debugging())
                    dbgPrintln("handleQueryResult() created e (speIndex==0).\n");
                throw new SPException(this, "SP_ERROR returned without exception!");
            }
        }

        throw new SPException(this, "internal Jasper error: unknown arg to handleQueryResult()");
    }

    // [PM] 3.9 called by glue
    SPTerm newGlueTerm() {
        if (debugging())
            SICStus.dbgPrintln("Calling " + this + ".newGlueTerm()");
        return new SPTerm(this, (long) 0 /*
                                          * used as special flag, note the importance of 'long'
                                          */);
    }

    // [PM] 3.9 called by glue
    SPTerm newGlueTerm(long nativeTermRef) {
        if (debugging())
            SICStus.dbgPrintln("Calling newGlueTerm(" + nativeTermRef + ")");
        return new SPTerm(this, nativeTermRef);
    }

    // [PM] 3.9 called by glue
    SPCanonicalAtom newGlueAtom(String name) throws AtomRegisterFailure // cannot
                                                                        // happen
                                                                        // (never
                                                                        // thrown)
    {
        if (debugging())
            SICStus.dbgPrintln("Calling newGlueAtom(\"" + name + "\")");
        return new SPCanonicalAtom(this, name);
    }

    // 3.9 Caller should have synchronized on this
    boolean isEmptyList(long termRef) {
        return spIsEmptyList(termRef);
    }

    /**
     * Finds the first solution to a query. If you need more than one solution, use
     * {@link SICStus#openQuery openQuery}.
     *
     * @param module
     *            The module used for the query.
     * @param name
     *            The predicate name used for the query.
     * @param args
     *            The arguments to the predicate. The number of arguments must match
     *            the functor of the predicate.
     * @return true/false corresponding to success/failure of the query.
     * @throws SPException
     *             If a Prolog exception was thrown.
     * @throws IllegalTermException
     *             and invalid term was detected and invalid term was detected
     * @see SICStus#openQuery
     * @see SPException
     * @see SPTerm
     *
     */
    public boolean query(String module, String name, SPTerm args[]) throws SPException, IllegalTermException // ,
                                                                                                             // IllegalCallerException
    {
        checkLegalCaller();
        synchronized (this) { // This restriction is really unfortunate but
                              // (almost) unavoidable
            return handleQueryResult(spCall(module, name, termArrayToLongArray(args)));
        }
    }

    /**
     * Finds the first solution to a query specified as a string. If you need more
     * than one solution, use {@link SICStus#openQuery openQuery}.
     *
     * @param string
     *            The goal to use for the query, with terminating period.
     * @param varMap
     *            The arguments to the predicate as a map from variable names to
     *            SPTerm objects. On success varMap will get entries added for all
     *            variables (with names not starting with underscore) in the goal
     *            that were not already present as input. May be null.
     *
     *            Allocates one {@link SPTerm SPTerm} for each entry added to the
     *            map (and, possibly, a small number of SPTerm objects for internal
     *            use). If varMap is null then this method behaves as if no SPTerm
     *            objects were created.
     *
     * @return True/false corresponding to success/failure of the query.
     * @throws SPException
     *             If a Prolog exception was thrown.
     * @see #openQuery
     * @see #queryCutFail
     * @see SPTerm
     */
    @Override
    public boolean query(String string, Map<String, ? extends Term> varMap) throws SPException {
        checkLegalCaller();

        // Cast it to the type we would have liked to use in the parameter list.
        @SuppressWarnings("unchecked")
        Map<String, Term> varTermMap = (Map<String, Term>) varMap;
        {
            long jstreamCode;
            boolean result;

            synchronized (this) {

                jstreamCode = openStringStream(string);
                if (debugging(2))
                    SICStus.dbgPrintln("queryFromString: jstreamCode==" + jstreamCode);
                try {
                    Set<SPTerm> freeList = newFreeList();
                    SPTerm streamCode;
                    SPTerm varsIn;
                    SPTerm varsOut;
                    SPQuery context = null;
                    boolean mapIsReadOnly;

                    mapIsReadOnly = (varTermMap == null || false /*
                                                                  * need a way to detect un-put-able maps
                                                                  */);

                    if (mapIsReadOnly) {
                        context = openContext();
                        if (debugging())
                            SICStus.dbgPrintln("creating context age()=" + context.age());
                    }
                    try {
                        streamCode = newTerm(freeList).putInteger(jstreamCode);
                        varsIn = ((varTermMap != null) ? toKeyList(varTermMap, freeList) : newTerm(freeList));
                        varsOut = newTerm(freeList).putVariable(); // -VarsOut

                        // jasper_from_string_query(+StreamCode, +VarsIn, -VarsOut)
                        result = query("prolog", "jasper_from_string_query",
                                new SPTerm[] { streamCode, varsIn, varsOut });
                        // streamCode, varsIn can be reused here
                        freeTerm(freeList, streamCode);
                        freeTerm(freeList, varsIn);

                        if (result && !mapIsReadOnly) { // add new variables to
                                                        // varMap on success
                            mergeKeyList(varTermMap, varsOut, freeList);
                        }
                        freeTerm(freeList, varsOut);
                    } finally {
                        if (context != null) {
                            context.cut(); // reclaim SP_term_refs
                        }
                    }

                } finally {
                    closeStringStream(jstreamCode);
                }
                return result;
            }
        }
    }

    /**
     * Finds the first solution to a query. If you need more than one solution, use
     * {@link SICStus#openQuery openQuery}.
     *
     * @param pred
     *            The predicate object to use for the query.
     * @param args
     *            The arguments to the predicate. The number of arguments must match
     *            the functor of the predicate.
     * @return True/false corresponding to success/failure of the query.
     * @throws SPException
     *             If a Prolog exception was thrown.
     * @throws IllegalTermException
     *             and invalid term was detected and invalid term was detected
     * @see SICStus#openQuery
     * @see SPException
     * @see SPPredicate
     * @see SPTerm
     * @deprecated Use module and predicate name directly instead
     *             ({@link #query(String, String, SPTerm[])}).
     *
     */
    @Deprecated
    public boolean query(SPPredicate pred, SPTerm args[]) throws SPException, IllegalTermException {
        checkLegalCaller();
        checkCallArity(pred, args.length);

        return query(pred.getModule(), pred.getName(), args);
    }

    /**
     * Finds the first solution to a query, then cuts away any choicepoints and
     * fails, i.e. ignores everything but the side-effects during the first
     * solution.
     *
     * @param module
     *            The module used for the query.
     * @param name
     *            The predicate name used for the query.
     * @param args
     *            The arguments to the predicate.
     * @return True/false corresponding to success/failure of the query.
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SPException
     */
    public boolean queryCutFail(String module, String name, SPTerm args[]) throws SPException {
        checkLegalCaller(); // 3.9
        synchronized (this) { // This restriction is really unfortunate but
                              // (almost) unavoidable
            return handleQueryResult(spCallCutFail(module, name, termArrayToLongArray(args)));
        }
    }

    /**
     * Finds the first solution to a query specified as a string, then cuts away any
     * choice points and fails, i.e. ignores everything but the side-effects during
     * the first solution.
     *
     * @param string
     *            The goal to use for the query, with terminating period.
     * @param varMap
     *            The arguments to the predicate as a map from variable names to
     *            SPTerm objects. May be null. No bindings are added.
     *
     * @return True/false corresponding to success/failure of the query.
     * @see SICStus#query
     *
     */
    @Override
    public boolean queryCutFail(String string, Map<String, ? extends Term> varMap) throws SPException {
        checkLegalCaller();
        // Cast it to the type we would have liked to use in the parameter list.
        @SuppressWarnings("unchecked")
        Map<String, Term> varTermMap = (Map<String, Term>) varMap;

        return queryCutFailFromString(string, varTermMap);
    }

    /**
     * Finds the first solution to a query, then cuts away any choicepoints and
     * fails, i.e. ignores everything but the side-effects during the first
     * solution.
     *
     * @param pred
     *            The predicate object used for the query.
     * @param args
     *            The arguments to the predicate. The number of arguments must match
     *            the functor of the predicate.
     * @return True/false corresponding to success/failure of the query.
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SPException
     * @see SPPredicate
     * @deprecated Use module and predicate name directly instead
     *             ({@link #queryCutFail(String, String, SPTerm[])}).
     *
     */
    @Deprecated
    public boolean queryCutFail(SPPredicate pred, SPTerm args[]) throws SPException {
        checkLegalCaller();
        checkCallArity(pred, args.length);

        return queryCutFail(pred.getModule(), pred.getName(), args);
    }

    /**
     * Opens a query for obtaining multiple solutions. The method itself does not
     * find any solutions; use the method {@link SPQuery#nextSolution nextSolution}
     * to do that. When no more solutions are needed, the query must be closed using
     * methods {@link SPQuery#close close} or {@link SPQuery#cut cut} on the
     * query-object.
     * <p>
     * Multiple queries can be open at the same time. See
     * {@link SPQuery#nextSolution nextSolution} for details and restrictions.
     *
     * @param module
     *            The module used for the query.
     * @param name
     *            The predicate name used for the query.
     * @param args
     *            The arguments to the predicate. The number of arguments must match
     *            the functor of the predicate.
     *
     * @return The opened {@link SPQuery query} object.
     * @throws IllegalTermException
     *             and invalid term was detected
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SPQuery#nextSolution
     * @see SPQuery#close
     * @see SPQuery#cut
     */
    public SPQuery openQuery(String module, String name, SPTerm args[]) throws IllegalTermException, SPException {

        checkLegalCaller(); // 3.9
        synchronized (this) {
            long mark = spGetTermRefs(); // before spOpenCall
            long qid = spOpenCall(module, name, termArrayToLongArray(args));

            if (qid == SPQuery.SP_ILLEGAL_QID) {
                throw new SPException(this, "Error from spOpenCall");
            }
            return new SPQuery(this, qid, mark);
        }
    }

    /**
     * Opens a query, specified as a string, for obtaining multiple solutions.
     *
     * The method itself does not find any solutions; use the method
     * {@link SPQuery#nextSolution nextSolution} to do that. When no more solutions
     * are needed, the query must be closed using methods {@link SPQuery#close
     * close} or {@link SPQuery#cut cut} on the query-object.
     *
     * <p>
     *
     * Multiple queries can be open at the same time. See
     * {@link SPQuery#nextSolution nextSolution} for details and restrictions.
     *
     * <p>
     *
     * Allocates one {@link SPTerm SPTerm} for each entry added to the map (and,
     * possibly, a small number of SPTerm objects for internal use). If varMap is
     * null then this method behaves as if no SPTerm objects were created. When the
     * query is closed (or cut) then all SPTerm objects are reclaimed.
     *
     * @param string
     *            The goal to use for the query, with terminating period.
     * @param varMap
     *            The arguments to the predicate as a map from variable names to
     *            SPTerm objects. The map will get entries added for all variables
     *            (with names not starting with underscore) in the goal that were
     *            not already present as input. May be null.
     *
     * @return The opened {@link SPQuery query} object.
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SPQuery#nextSolution
     * @see SPQuery#close
     * @see SPQuery#cut
     *
     */
    public SPQuery openQuery(String string, Map<String, ? extends Term> varMap) throws SPException {
        checkLegalCaller();
        // Cast it to the type we would have liked to use in the parameter list.
        @SuppressWarnings("unchecked")
        Map<String, Term> varTermMap = (Map<String, Term>) varMap;

        return openQueryFromString(string, varTermMap);
    }

    /**
     * Opens a query for obtaining multiple solutions. The method itself does not
     * find any solutions; use the method {@link SPQuery#nextSolution nextSolution}
     * to do that. When no more solutions are needed, the query must be closed using
     * methods {@link SPQuery#close close} or {@link SPQuery#cut cut} on the
     * query-object.
     * <p>
     * Multiple queries can be open at the same time. See
     * {@link SPQuery#nextSolution nextSolution} for details and restrictions.
     *
     * @param pred
     *            The predicate-object to open a query on.
     * @param args
     *            The arguments to the predicate. The number of arguments must match
     *            the functor of the predicate.
     *
     * @return The opened {@link SPQuery query} object.
     * @throws IllegalTermException
     *             and invalid term was detected
     * @throws SPException
     *             if an Prolog exception was thrown
     * @see SPQuery#nextSolution
     * @see SPQuery#close
     * @see SPQuery#cut
     * @see SPPredicate
     * @deprecated Use module and predicate name directly instead
     *             ({@link #openQuery(String, String, SPTerm[])}).
     */
    @Deprecated
    public SPQuery openQuery(SPPredicate pred, SPTerm args[]) throws IllegalTermException, SPException {
        checkLegalCaller();

        checkCallArity(pred, args.length);
        return openQuery(pred.getModule(), pred.getName(), args);
    }

    @SuppressWarnings("deprecation")
    private void checkCallArity(SPPredicate pred, int arity) throws SPException {
        if (pred.getArity() != arity) {
            throw new SPException(this, pred.getModule() + ":" + pred.getName() + "/" + pred.getArity()
                    + " called with " + arity + "arguments");
        }
    }

    void resetTermRefs(long mark) {
        spResetTermRefs(mark);
    }

    /* not yet public */SPQuery openContext() throws SPException {
        checkLegalCaller();

        // Called from SPPredicate constructor so cannot use openQuery()
        synchronized (this) {
            long mark = spGetTermRefs(); // before spOpenContext();
            long qid = spOpenContext(); // a.k.a. openQuery(prolog:true)

            if (qid == SPQuery.SP_ILLEGAL_QID) {
                throw new SPException(this, "Error from spOpenContext");
            }
            return new SPQuery(this, qid, mark);
        }
    }

    // Push a faked query on the query stack. Used to keep track of positions in
    // the SPTerm stack.
    // Called by Glue_PushContext and by initSICStus
    // must be called while 'synchronized (this)'.
    void pushQueryMark() throws SPException {
        SPQuery fakeQuery;
        // synchronized (this) {
        // Hack. We use faked queries to keep track of term_stack positions.
        fakeQuery = new SPQuery(this, SPQuery.SP_ILLEGAL_QID);
        if (debugging()) {
            SICStus.dbgPrintln("pushQueryMark: term_stack_mark.nativePredRef="
                    + ((fakeQuery.term_stack_mark == null) ? -42 : fakeQuery.term_stack_mark.nativeTermRef));
            if (debugging(2)) {
                query_stack.dumpQueryStack();
                if (term_stack != null)
                    term_stack.dumpTermStack();
            }
        }

        if (fakeQuery != query_stack) {
            throw new SPException(this, "internal Jasper error: (fakeQuery != query_stack)");
        }
        // }
    }

    // 3.8.5 Called by splfr-generated Java glue code before method call
    void Glue_PushContext() throws SPException {
        synchronized (this) {
            pushQueryMark();
            pushSPGlobal();
        }
    }

    void pushSPGlobal() {
        Stack<SICStus> stack = spGlobalStack.get();

        if (debugging(2)) {
            SICStus.dbgPrintln("Enter pushSPGlobal()" + ", this==" + this + ", stack==" + stack + ", thread=="
                    + Thread.currentThread());
        }

        if (stack == null) {
            stack = new Stack<>();
            spGlobalStack.set(stack);
        }
        stack.push(this);
        defaultSPGlobal = this;
    }

    static SICStus topSPGlobal() {
        Stack<SICStus> stack = spGlobalStack.get();
        if (debugLevelValueOrig > 0) { // [PM] ugly but have no static debugging() method
            SICStus.dbgPrintln("Enter topSPGlobal()" + ", stack==" + stack + ", thread==" + Thread.currentThread());
        }

        if (stack == null) {
            return null;
        }
        return stack.peek();
    }

    void popSPGlobal() {
        Stack<SICStus> stack = spGlobalStack.get();

        if (debugging(2)) {
            SICStus.dbgPrintln("Enter popSPGlobal()" + ", this==" + this + ", stack==" + stack + ", thread=="
                    + Thread.currentThread());
        }

        if (stack == null) {
            if (debugging())
                SICStus.dbgPrintln("ERROR: spGlobalStack==null this==" + this + ", thread==" + Thread.currentThread());
        }
        SICStus tmp = stack.pop();
        // defaultSPGlobal should never be re-set.

        if (tmp != this) {
            if (debugging())
                SICStus.dbgPrintln("ERROR: spGlobalStack.pop() != this (" + tmp + "!=" + this + "), thread=="
                        + Thread.currentThread());
        }
    }

    // 3.8.5 Called by splfr-generated Java glue code after method call
    // NOTE: consider closing still opened queries instead of giving error?
    void Glue_PopContext() throws SPException {
        synchronized (this) {
            popSPGlobal();
            if (query_stack.nativeQueryRef != SPQuery.SP_ILLEGAL_QID) {
                throw new SPException(this, "internal Jasper error: (popping non-context (forget to close query?))");
            }
            if (debugging(2)) {
                SICStus.dbgPrintln("Before query_stack.cut()");
                query_stack.dumpQueryStack();
                if (term_stack != null)
                    term_stack.dumpTermStack();
            }

            query_stack.cut();
            if (debugging(2)) {
                SICStus.dbgPrintln("After query_stack.cut()");
                query_stack.dumpQueryStack();
                if (term_stack != null)
                    term_stack.dumpTermStack();
            }

        }
    }

    // 3.8.5 Called by jasper.c call_(static/instance)_C before the method call
    void MetaGlue_PushContext() throws SPException {
        Glue_PushContext();
    }

    // 3.8.5 Called by jasper.c call_(static/instance)_C after the method call
    void MetaGlue_PopContext() throws SPException {
        Glue_PopContext();
    }

    /*
     *
     * ** Everything below is experimental**** * Perhaps slow.** * Perhaps leaks.
     */

    /* Read from string support */

    // Implement a free list for SPTerm objects.
    // By default reuseTermRefs is on and null will be returned. This
    // signals the other functions below to use the reuseTermRefs
    // functionality. If on the other hand reuseTermRefs is off then
    // this implies the user forced it off and the functions below
    // will instead fall back on maintaining an explicit free-list.
    Set<SPTerm> newFreeList() {
        if (reuseTermRefs()) {
            // Tells the others to use built-in reuseTermRefs functionality
            return null;
        } else {
            // Tells the other to use the old-style free-list
            return new HashSet<>();
        }
    }

    SPTerm newTerm(Set<SPTerm> freeList) {
        if (freeList != null) { // old style free-list forced
            if (!freeList.isEmpty()) {
                Iterator<SPTerm> itr = freeList.iterator();

                while (itr.hasNext()) {
                    SPTerm term = itr.next();
                    itr.remove();
                    if (term.isValid()) {
                        // done in reuseTerm
                        // term.putString("[]");
                        return term;
                    }
                }
            }
            return new SPTerm(this);
        } else {
            return new SPTerm(this);
        }
    }

    void freeTerm(Set<SPTerm> freeList, SPTerm term) throws SPException {
        if (freeList != null) {
            if (freeList != null && term.isValid(this)) {
                term.putString("[]");
                freeList.add(term);
            }
        } else {
            term.delete(true);
        }
    }

    SPTerm toKeyList(Map<String, Term> varMap, Set<SPTerm> freeList) throws SPException {
        SPTerm keyList = newTerm(freeList);

        if (!varMap.isEmpty()) {
            Iterator<Entry<String, Term>> itr = varMap.entrySet().iterator();
            SPTerm tmp = newTerm(freeList); // Key

            while (itr.hasNext()) {
                Entry<String, Term> entry = itr.next();

                if (debugging())
                    SICStus.dbgPrintln("toKeyList: entry.getKey()==" + entry.getKey());
                tmp.putString(entry.getKey());
                tmp.consFunctor("=", new SPTerm[] { tmp, (SPTerm) entry.getValue() }); // Key-Value
                keyList.consList(tmp, keyList);
            }
        }
        return keyList;
    }

    void mergeKeyList(Map<String, Term> varMap, SPTerm keyList, Set<SPTerm> freeList) throws SPException {
        SPTerm tail = newTerm(freeList).putTerm(keyList);
        SPTerm head = newTerm(freeList);
        SPTerm name = newTerm(freeList);
        SPTerm var = newTerm(freeList);

        while (tail.isList()) {
            String key;

            tail.getList(head, tail);
            // head is "NAME"-Variable

            head.getArg(1, name);
            key = name.getString();

            // If variable already in varMap then unify regardless of name
            // If not in varMap then only add if the name does not start with
            // underscore
            if (varMap.containsKey(key)) {
                head.getArg(2, var);
                var.unify(varMap.get(key));
            } else if (!key.startsWith("_")) {
                SPTerm newVar = newTerm(freeList);
                head.getArg(2, newVar);
                varMap.put(key, newVar);
            }
        }
        freeTerm(freeList, tail);
        freeTerm(freeList, head);
        freeTerm(freeList, name);
        freeTerm(freeList, var);
    }

    private long openStringStream(String string) {
        /*
         * [PM] 3.9b5 the string stream handling has a global list shared by all SICStus
         * run-times. We serialize access to this list by always locking the whole class
         */
        synchronized (this.getClass()) {
            return spOpenStringStream(string);
        }
    }

    boolean closeStringStream(long streamCode) {
        /*
         * [PM] 3.9b5 the string stream handling has a global list shared by all SICStus
         * run-times. We serialize access to this list by always locking the whole class
         */
        synchronized (this.getClass()) {
            return spCloseStringStream(streamCode);
        }
    }

    /**
     *
     * @param stringBuf
     *            This is a {@link StringBuffer} (rather than a
     *            {@link StringBuilder}) for legacy reasons.
     * @return
     */
    /* package */
    long openStringBufferStream(StringBuffer stringBuf) {
        synchronized (this.getClass()) {
            return spOpenStringBufferStream(stringBuf);
        }
    }

    /**
     * Creates a term by reading from a string.
     *
     * @param string
     *            The printed representation of the term, with terminating period.
     * @param varMap
     *            Bindings for any variables occurring in the term, as a map from
     *            variable names to SPTerm objects. The map will get entries added
     *            for all variables (with names not starting with underscore) in the
     *            term that were not already present as input. May be null.
     *
     *            Allocates one {@link SPTerm SPTerm} for each entry added to the
     *            map (and, possibly, a small number of SPTerm objects for internal
     *            use). If varMap is null then this method behaves as if only the
     *            returned SPTerm object were created.
     * @return the read term
     * @throws SPException
     *             if an Prolog exception was thrown
     */
    public SPTerm readFromString(String string, Map<String, ? extends Term> varMap) throws SPException {
        long jstreamCode;
        SPTerm term;
        // Cast it to the type we would have liked to use in the parameter list.
        @SuppressWarnings("unchecked")
        Map<String, Term> varTermMap = (Map<String, Term>) varMap;

        checkLegalCaller(); // 3.9

        synchronized (this) {
            jstreamCode = openStringStream(string);
            try {
                Set<SPTerm> freeList = newFreeList();
                SPTerm varsIn;
                SPTerm varsOut;
                SPTerm streamCode;

                SPQuery context = null;
                boolean mapIsReadOnly;

                term = newTerm(freeList).putVariable(); // -Term (outside
                                                        // context)

                mapIsReadOnly = (varTermMap == null || false /*
                                                              * need a way to detect un-put-able maps
                                                              */);

                if (mapIsReadOnly) {
                    context = openContext();
                }
                try {
                    streamCode = newTerm(freeList).putInteger(jstreamCode);
                    varsIn = ((varTermMap != null) ? toKeyList(varTermMap, freeList) : newTerm(freeList));
                    varsOut = newTerm(freeList).putVariable(); // -VarsOut

                    // jasper_read_from_string(+StreamCode, +VarsIn, -VarsOut,
                    // -Term)
                    if (!query("prolog", "jasper_read_from_string",
                            new SPTerm[] { streamCode, varsIn, varsOut, term })) {
                        throw new ConversionFailedException(this, "prolog:jasper_read_from_string/4 failed");
                    }
                    // streamCode, varsIn can be reused here
                    freeTerm(freeList, streamCode);
                    freeTerm(freeList, varsIn);

                    if (!mapIsReadOnly) { // add new variables to varMap
                        mergeKeyList(varTermMap, varsOut, freeList);
                    }
                } finally {
                    if (context != null) {
                        context.cut();
                    }
                }

                freeTerm(freeList, varsOut);
            } finally {
                closeStringStream(jstreamCode);
            }
            return term;
        }
    }

    /**
     * Equivalent to readFromString(string, null);
     *
     * @param string
     *            The printed representation of the term, with terminating period.
     * @return the read term
     * @throws SPException
     *             if an Prolog exception was thrown
     */
    public SPTerm readFromString(String string) throws SPException {
        checkLegalCaller(); // 3.9
        return readFromString(string, null);
    }

    /**
     * This is to {@link #queryCutFail} what {@link #queryFromString} is to
     * {@link #query}.
     *
     * @param string
     *            The goal to use for the query, with terminating period.
     * @param varMap
     *            The arguments to the predicate as a map from variable names to
     *            {@ink SPTerm} objects. May be null. No bindings are added.
     */
    boolean queryCutFailFromString(String string, Map<String, Term> varMap) throws SPException {
        long jstreamCode;
        boolean result;
        checkLegalCaller();

        synchronized (this) {

            jstreamCode = openStringStream(string);
            try {
                Set<SPTerm> freeList = newFreeList();
                SPQuery context;
                SPTerm varsIn;
                SPTerm streamCode;

                context = openContext();
                try {

                    streamCode = newTerm(freeList).putInteger(jstreamCode);
                    varsIn = ((varMap != null) ? toKeyList(varMap, freeList) : newTerm(freeList));

                    // jasper_from_string_query_cut_fail(+StreamCode, +VarsIn)
                    result = queryCutFail("prolog", "jasper_from_string_query_cut_fail",
                            new SPTerm[] { streamCode, varsIn });
                    // streamCode, varsIn can be reused here
                    freeTerm(freeList, streamCode);
                    freeTerm(freeList, varsIn);
                } finally {
                    context.close();
                }
            } finally {
                closeStringStream(jstreamCode);
            }
            return result;
        }
    }

    /**
     * This is to {@link #openQuery} what {@link #queryFromString} is to
     * {@link #query}.
     *
     * Allocates one {@link SPTerm SPTerm} for each entry added to the map (and,
     * possibly, a small number of SPTerm objects for internal use). If varMap is
     * null then this method behaves as if no {@link SPTerm} objects were created.
     * When the query is closed (or cut) then all {@link SPTerm} objects are
     * reclaimed.
     *
     * @param string
     *            The goal to use for the query, with terminating period.
     * @param varMap
     *            The arguments to the predicate as a map from variable names to
     *            {@link SPTerm} objects. The map will get entries added for all
     *            variables (with names not starting with underscore) in the goal
     *            that were not already present as input. May be null.
     * @return The opened {@link SPQuery query}.
     */
    private SPQuery openQueryFromString(String string, Map<String, Term> varMap) throws SPException {
        SPQuery context;
        SPQuery query;
        SPTerm goal;
        synchronized (this) { // protect time span between openContext and
                              // setContext
            context = openContext();
            try {
                // DBG
                if (debugging(2))
                    dbgPrintln("openQueryFromString readFromString(" + string + "), varMap==" + varMap);

                goal = readFromString(string, varMap);
                // DBG
                if (debugging(2))
                    dbgPrintln("openQueryFromString goal==" + goal + ", varMap==" + varMap);
                query = openQuery("prolog", "jasper_call", new SPTerm[] { goal });
                query.setContext(context);
                context = null; // tell finally clause that context is installed
                return query;
            } finally { // really wanted catch(Exception e){close();throw e;}
                // DBG
                if (debugging(2))
                    dbgPrintln("openQueryFromString finally context==" + context);
                if (context != null) {
                    context.close();
                }
            }
        }
    }

    // ==============================================================
    // Interface Prolog

    @Override
    public Query openPrologQuery(String string, Map<String, ? extends Term> varMap) throws SPException {
        return openQuery(string, varMap);
    }

    @Override
    public Term prologReadFromString(String string, Map<String, ? extends Term> varMap) throws SPException {
        // Cast it to the type we would have liked to use in the parameter list.
        @SuppressWarnings("unchecked")
        Map<String, Term> varTermMap = (Map<String, Term>) varMap;

        return readFromString(string, varTermMap);
    }

    // [PD] 3.9 Implementing newTerm methods from interface Prolog
    @Override
    public Term newTerm() {
        return new SPTerm(this);
    }

    @Override
    public Term newTerm(Term t) throws IllegalTermException, ConversionFailedException {
        return new SPTerm(this, (SPTerm) t);
    }

    /**
     * @param i
     *            initial value of the term
     * @return a term representation of the argument
     */
    public Term newTerm(Integer i) {
        return newTerm(i.intValue());
    }

    @Override
    public Term newTerm(int i) {
        return new SPTerm(this, i);
    }

    /**
     * @param j
     *            initial value of the term
     * @return a term representation of the argument
     * @throws IllegalTermException
     *             an invalid term was detected
     * @throws ConversionFailedException
     *             the argument could not be converted to a term
     */
    public Term newTerm(Long j) throws IllegalTermException, ConversionFailedException {
        return newTerm(j.longValue());
    }

    @Override
    public Term newTerm(long j) throws IllegalTermException, ConversionFailedException {
        return new SPTerm(this).putInteger(j);
    }

    /**
     * @param d
     *            the non-null value
     * @return a term representation of the argument
     */
    public Term newTerm(Double d) {
        return newTerm(d.doubleValue());
    }

    @Override
    public Term newTerm(double d) {
        return new SPTerm(this, d);
    }

    /**
     * @param f
     *            initial value of the term
     * @return a term representation of the argument
     */
    public Term newTerm(Float f) {
        return newTerm(f.floatValue());
    }

    @Override
    public Term newTerm(float f) {
        return new SPTerm(this, f);
    }

    @Override
    public Term newTerm(String a) throws ConversionFailedException {
        return new SPTerm(this, a);
    }

    @Override
    public Term newTerm(String functor, Term args[]) throws ConversionFailedException, IllegalTermException {
        SPTerm[] spta = new SPTerm[args.length];
        for (int i = 0; i < args.length; i++) {
            spta[i] = (SPTerm) args[i];
        }
        return new SPTerm(this, functor, spta);
    }

    // [PD] 3.9 Implementing newVariable method from interface Prolog
    @Override
    public Term newVariable() throws ConversionFailedException, IllegalTermException {
        SPTerm t = new SPTerm(this);
        return t.putVariable();
    }

    // [PD] 3.9 Implementing consFunctor method from interface Prolog
    @Override
    public Term consFunctor(String functor, Term[] args) throws ConversionFailedException, IllegalTermException {
        return newTerm(functor, args);
    }

    // [PD] 3.9 Implementing consList method from interface Prolog
    @Override
    public Term consList(Term head, Term tail) throws ConversionFailedException, IllegalTermException {
        SPTerm t = new SPTerm(this);
        return t.consList((SPTerm) head, (SPTerm) tail);
    }

    // [PD] 3.9.2 Implementing newObjectTerm method from interface Prolog
    @Override
    public Term newObjectTerm(Object obj) throws IllegalTermException, ConversionFailedException {
        SPTerm t = new SPTerm(this);
        return t.putObject(obj);
    }

    // [PD] 3.9.2 Implementing numberFromString method from interface Prolog
    @Override
    public Term numberFromString(String str) throws ConversionFailedException, IllegalTermException {
        SPTerm t = new SPTerm(this);
        return t.putNumberChars(str);
    }

    // [PD] 3.9.2 Implementing listFromString method from interface Prolog
    @Override
    public Term listFromString(String str) throws ConversionFailedException, IllegalTermException {
        SPTerm t = new SPTerm(this);
        return t.putListChars(str);
    }

    // ==============================================================
    // [PD] 3.9 Creating a Prolog client and starting a Prolog server

    private Server myServer = null;
    private Prolog myProlog = null;

    /**
     * Returns the Prolog interface for this SICStus object. If a client object
     * already exists, return that object, else create a server and a client (
     * {@link Prolog}) interface for this SICStus object, and return the new client.
     * The server may be started by calling {@link SICStus#startServer}
     *
     * @return a prolog instance
     * @throws InterruptedException
     *             if the thread was interrupted
     */
    synchronized public Prolog newProlog() throws InterruptedException {
        if (myServer == null) {
            myProlog = Jasper.newProlog(this);
            myServer = myProlog.getServer();
        }
        return myProlog;
    }

    /**
     * Returns the SICStus object corresponding to the SICStus runtime that called
     * us.
     *
     * @return the caller
     */
    public static SICStus getCaller() {
        return getInitializedSICStus();
    }

    /**
     * Starts serving requests from a Prolog client. This method does not return
     * until another thread calls {@link SICStus#stopServer}. Before calling this
     * method you should call {@link SICStus#newProlog} and hand the result over to
     * another {@link java.lang.Thread}.
     */
    public void startServer() {
        myServer.run();
    }

    /**
     * Stops the server. Calling this method causes the {@link java.lang.Thread}
     * running in the {@link SICStus#startServer} method to return.
     */
    public void stopServer() {
        myServer.stopServer();
    }

    /**
    *
    */
    @Override
    public Server getServer() {
        return myServer;
    }

    /**
     * For internal use by Jasper.
     *
     * @param s
     *            Undocumented
     */
    public void setServer(Server s) {
        myServer = s;
    }

    /**
     * Does nothing.
     */
    public void init() {
    }

}
