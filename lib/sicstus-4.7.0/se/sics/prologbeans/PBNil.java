/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

/**
 * Representation of the empty list, {@code []}.
 */

class PBNil extends PBAtom {

    PBNil() {
        super("[]");
    }

    @Override
    public boolean isEmptyList() {
        return true;
    }

    @Override
    public boolean isProperList() {
        return true;
    }

    @Override
    public int length() {
        return 0;
    }

    @Override
    public String getString() {
        return "";
    }
}
