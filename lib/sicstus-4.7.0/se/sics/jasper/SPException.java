/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

import java.util.HashMap;

/**
 * Represents a exception thrown by Jasper. You should not create or inherit
 * from <code>SPException</code> or its sub-classes.
 */
public class SPException extends Exception implements PrologException {
    private static final long serialVersionUID = 1L;

    private final SICStus iSP;
    @SuppressWarnings("unused")
    private final int iErrNo;
    private final String desc;
    private final String iToStringString;

    /**
     * Index which was used when the exception term was asserted.
     * <p>
     * It is assumed that only one (live) instance refers to this index. This means
     * that the index can be reclaimed (an dthe corresponding exception term can be
     * retracted) when this instance is reclaimed by the GC.
     */
    private final int iAssertIndex;

    private SPException(SICStus sp, int errNo, String spec, int termIndex, String termString) {
        if (sp.debugging(2)) {
            SICStus.dbgPrintln("SPException.initSPException():");
            SICStus.dbgPrintln("    sp==" + sp);
            SICStus.dbgPrintln("    err_no==" + errNo);
            SICStus.dbgPrintln("    spec==" + spec);
            SICStus.dbgPrintln("    termIndex==" + termIndex);
            SICStus.dbgPrintln("    termString==" + termString);
        }

        iSP = sp;
        iErrNo = errNo;
        desc = spec;
        iAssertIndex = termIndex;

        String toStringString = null;

        if (termString != null) {
            toStringString = termString;
        } else {
            String errdesc = null;
            if (errNo != 0) {
                synchronized (sp) {
                    errdesc = sp.spErrorMessage(errNo);
                }
                if ("".equals(errdesc)) {
                    errdesc = "Error #" + errNo;
                }
            }

            if (spec != null) {
                if (errdesc != null) {
                    toStringString = desc + ": >" + errdesc + "<";
                } else {
                    toStringString = desc + ": <unknown>";
                }
            } else {
                toStringString = errdesc;
            }
        }

        iToStringString = toStringString;
    }

    SPException(SICStus sp, int errNo, String spec) {
        this(sp, errNo, spec, 0, null);
    }

    /**
     * Creates an exception with the given specification.
     * 
     * @param spec
     *            Description of the error.
     */
    SPException(SICStus sp, String spec) {
        this(sp, getErrno(sp), spec, 0, null);
    }

    /**
     * Creates an exception containing a Prolog term. Prolog exceptions thrown by
     * <code>se.sics.jasper.SICStus#query</code> will be of this type.
     */

    SPException(SICStus sp, String termString, int termIndex) {
        this(sp, (termIndex == 0 ? getErrno(sp) : 0), null, termIndex, termString);
    }

    /**
     * Get the {@link SICStus#spGetErrno() error number} from the supplied SICStus.
     * Returns 0 if {@code sp} is null, not initialized, or if we are
     * {@link SICStus#isLegalCaller() not allowed to call} it.
     * 
     * @param sp
     *            the {@link SICSTus} instance, or {@code null}.
     */
    private static int getErrno(SICStus sp) {
        // [PM] 3.9.2b3 Avoid spGetErrno() from wrong caller. E.g., when creating
        // IllegalCallerException...
        if (sp.isLegalCaller() && sp.initialized) {
            synchronized (sp) {
                return sp.spGetErrno();
            }
        }
        return 0;
    }

    /**
     * Returns a description of the exception. If an error-description was given, it
     * is returned together with the error-string returned from Prolog, if any.
     */
    @Override
    public String toString() {
        if (iSP.debugging(2))
            SICStus.dbgPrintln("in SPException.toString()");
        return iToStringString;

    }

    /**
     * Returns the exception term. Returns null if there is no exception term.
     *
     */
    @Override
    public Term getTerm() throws SPException {
        if (getAssertIndex() == 0) {
            return null;
        }
        iSP.checkLegalCaller();
        HashMap<String, Term> varMap = new HashMap<>();
        iSP.query("prolog:'$SPException'(" + getAssertIndex() + ",Term).", varMap);
        return varMap.get("Term");
    }

    /* package */ int getAssertIndex() {
        return iAssertIndex;
    }

}

/*-
 * [PM] Keep the original indentation style
 * Local variables:
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * end:
 **/
