/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Used internally to signal errors from native code.
 *
 * <p>
 * You should not create or inherit from {@code NativeCodeException}.
 */
class NativeCodeException extends SPException {
    private static final long serialVersionUID = 1L;

    NativeCodeException(SICStus sp)// called from glue (spnative.c)
    {
        super(sp, (String) null);
    }
}
