/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;

/**
 * Representation of a Prolog non-compound term.
 */

abstract class PBAtomic extends PBTerm {

    final static int ATOM = 1;
    final static int INTEGER = 2;
    final static int FLOAT = 3;
    final static int VARIABLE = 4;

    /**
     * Creates a new instance with the specified name.
     */
    PBAtomic(String name) {
        super(name);
    }

    abstract int getType();

    @Override
    public boolean isAtom() {
        return false;
    }

    @Override
    public boolean isInteger() {
        return false;
    }

    @Override
    public boolean isFloat() {
        return false;
    }

    @Override
    public boolean isAtomic() {
        return true;
    }

    @Override
    abstract String toPrologString();

    @Override
    abstract public String toString();

    @Override
    void fastWrite(FastWriter writer) throws IOException {
        writer.writeAtomic(this);
    }

    @Override
    PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
        writer.writeAtomic(this);
        return PBTerm.NO_TERMS;
    }

}
