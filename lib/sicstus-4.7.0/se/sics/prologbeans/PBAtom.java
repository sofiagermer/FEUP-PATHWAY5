/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

/**
 * Representation of a Prolog atom.
 */

class PBAtom extends PBAtomic {
    /**
     * Creates a new <code>PBAtom</code> instance with the specified name.
     */
    PBAtom(String name) {
        super(name);
    }

    @Override
    int getType() {
        return ATOM;
    }

    @Override
    public boolean isAtom() {
        return true;
    }

    @Override
    String toPrologString() {
        return stuffAtom(name);
    }

    @Override
    public String toString() {
        return name;
    }

}
