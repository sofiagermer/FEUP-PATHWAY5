/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.math.BigInteger;

/**
 * Representation of a large Prolog integer.
 */

class PBBignum extends PBAtomic {
    BigInteger bigIntValue;

    /**
     * Creates a new instance with the specified value.
     */
    PBBignum(BigInteger value) {
        super(value.toString());
        bigIntValue = value;
    }

    PBBignum(BigInteger value, String name) {
        super(name);
        bigIntValue = value;
    }

    @Override
    int getType() {
        return INTEGER;
    }

    @Override
    public boolean isBignum() {
        return true;
    }

    @Override
    public BigInteger bigIntegerValue() {
        return bigIntValue;
    }

    @Override
    String toPrologString() {
        return toString();
    }

    @Override
    public String toString() {
        return bigIntValue.toString();
    }

}
