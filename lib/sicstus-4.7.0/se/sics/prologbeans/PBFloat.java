/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

/**
 * Representation of a Prolog floating point number.
 */

class PBFloat extends PBAtomic {
    private final double doubleValue;

    PBFloat(double value) {
        super(Double.toString(value));
        doubleValue = value;
    }

    PBFloat(double value, String name) {
        super(name);
        doubleValue = value;
    }

    @Override
    int getType() {
        return FLOAT;
    }

    @Override
    public boolean isFloat() {
        return true;
    }

    @Override
    public double floatValue() {
        return doubleValue;
    }

    @Override
    String toPrologString() {
        return toString();
    }

    @Override
    public String toString() {
        return Double.toString(doubleValue);
    }

}
