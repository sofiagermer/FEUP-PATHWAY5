/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Thrown if SICStus runtime is called from a thread which is not the creator of
 * the SICStus runtime.
 * <p>
 * See the class {@link se.sics.jasper.Jasper} for a way to call SICStus runtime
 * from multiple threads.
 * <p>
 * You should not create or inherit from {@code IllegalCallerException}.
 * <p>
 * You should not catch an {@code IllegalCallerException}. Catch
 * {@link SPException} instead, since {@code IllegalCallerException} is not
 * guaranteed to exist in future versions of Jasper.
 */
public class IllegalCallerException extends SPException {
    private static final long serialVersionUID = 1L;

    /**
     * Internal constructor.
     * 
     * @param sp
     * @param expected
     * @param found
     */
    IllegalCallerException(SICStus sp, Thread expected, Thread found) {
        super(sp, "illegal caller: expected thread " + expected + ", found " + found);
    }
}
