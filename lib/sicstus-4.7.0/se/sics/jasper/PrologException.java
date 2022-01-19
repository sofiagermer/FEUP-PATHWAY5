/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Represents an exception thrown by a Prolog object. User code should catch a
 * {@code PrologException}.
 */
public interface PrologException {
    @Override
    String toString();

    /**
     * @return the exception term
     * @throws Exception
     *             something went wrong
     */
    public Term getTerm() throws Exception;
}
