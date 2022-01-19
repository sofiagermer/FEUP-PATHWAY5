/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Thrown if {@link SPCanonicalAtom} could not register or unregister a
 * canonical atom representation.
 * <p>
 * You should not catch an {@code AtomRegisterFailure}. Catch
 * {@link SPException} instead, since {@code AtomRegisterFailure} is not
 * guaranteed to exist in future versions of Jasper.
 */
public class AtomRegisterFailure extends SPException {
    private static final long serialVersionUID = 1L;

    /**
     * Internal constructor.
     * 
     * @param sp
     * @param msg
     */
    AtomRegisterFailure(SICStus sp, String msg) {
        super(sp, msg);
    }
}
