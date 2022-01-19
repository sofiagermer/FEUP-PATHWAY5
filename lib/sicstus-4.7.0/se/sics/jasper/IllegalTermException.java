/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Thrown if trying to use a (no longer) valid {@link SPTerm}.
 *
 * <p>
 * You should not create or inherit from {@code IllegalTermException}.
 * <p>
 * You should not catch an {@code IllegalTermException}. Catch
 * {@link SPException} instead, since {@code IllegalTermException} is not
 * guaranteed to exist in future versions of Jasper.
 *
 * @see se.sics.jasper.SPTerm#isValid
 */
public class IllegalTermException extends SPException {
    private static final long serialVersionUID = 1L;

    IllegalTermException(SICStus sp) {
        super(sp, 0, "Illegal SPTerm");
    }
}
