/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

/**
 * Representation of a Prolog integer.
 */

class PBInteger extends PBAtomic {
    private final long longValue;

    PBInteger(long value) {
        super(Long.toString(value));
        longValue = value;
    }

    PBInteger(long value, String name) {
        super(name);
        longValue = value;
    }

    @Override
    int getType() {
        return INTEGER;
    }

    @Override
    public boolean isInteger() {
        return true;
    }

    @Override
    public long intValue() {
        return longValue;
    }

    @Override
    String toPrologString() {
        return toString();
    }

    @Override
    public String toString() {
        return Long.toString(longValue);
    }

}
