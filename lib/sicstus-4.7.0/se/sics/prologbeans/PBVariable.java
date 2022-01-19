/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;

/**
 * Representation of a Prolog variable.
 */

class PBVariable extends PBTerm {

    PBVariable(String name) {
        super(name);
    }

    int getType() {
        return PBAtomic.VARIABLE;
    }

    @Override
    public boolean isVariable() {
        return true;
    }

    @Override
    String toPrologString() {
        return toString();
    }

    @Override
    public String toString() {
        return name;
    }

    @Override
    void fastWrite(FastWriter writer) throws IOException {
        writer.writeVariable(this);
    }

    @Override
    PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
        writer.writeVariable(this);
        return PBTerm.NO_TERMS;
    }
}
