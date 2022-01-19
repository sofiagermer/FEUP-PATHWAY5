/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Thrown if a Java value cannot be converted to a Prolog term.
 * <p>
 * You should not catch a {@code ConversionFailedException}. Catch
 * {@link SPException} instead, since {@code ConversionFailedException} is not
 * guaranteed to exist in future versions of Jasper.
 * 
 * @see se.sics.jasper.SPTerm
 */
public class ConversionFailedException extends SPException {
    private static final long serialVersionUID = 1L;

    /**
     * Internal constructor.
     * 
     * @param sp
     *            the Prolog instance
     * @param msg
     *            the message
     */
    ConversionFailedException(SICStus sp, String msg) {
        super(sp, msg);
    }
}
