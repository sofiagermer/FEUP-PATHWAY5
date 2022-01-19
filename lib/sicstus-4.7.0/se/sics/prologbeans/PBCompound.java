/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;

/**
 * Representation of a Prolog compound term.
 */
class PBCompound extends PBTerm {
    protected final PBTerm[] arguments;

    PBCompound(String name, PBTerm[] args) {
        super(name);
        this.arguments = args;
    }

    @Override
    public boolean isAtom() {
        return getArity() == 0;
    }

    @Override
    public boolean isAtomic() {
        return getArity() == 0;
    }

    @Override
    public boolean isListCell() {
        return getArity() == 2 && ".".equals(getName());
    }

    @Override
    public boolean isProperList() {
        PBTerm tmp = this;

        while (tmp.isListCell()) {
            tmp = tmp.getArgument(2);
        }
        return tmp.isEmptyList();
    }

    @Override
    public PBTerm getArgument(int index) {
        if (arguments == null) {
            throw new IllegalStateException("not a compound term: " + toString());
        }
        if (index < 1 || index > arguments.length) {
            throw new IndexOutOfBoundsException("Index: " + index + ", needs to be between 1 and " + getArity());
        }
        return arguments[index - 1];
    }

    @Override
    public int getArity() {
        return arguments == null ? 0 : arguments.length;
    }

    @Override
    String toPrologString() {
        StringBuilder sb = new StringBuilder().append(stuffAtom(name));
        if (arguments != null) {
            sb.append('(');
            for (int i = 0, n = arguments.length; i < n; i++) {
                if (i > 0) {
                    sb.append(',');
                }
                sb.append(arguments[i].toPrologString());
            }
            sb.append(')');
        }
        return sb.toString();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder().append(name);
        if (arguments != null) {
            sb.append('(');
            for (int i = 0, n = arguments.length; i < n; i++) {
                if (i > 0) {
                    sb.append(',');
                }
                sb.append((arguments[i] != null ? arguments[i].toString() : "<<NULL>>"));
            }
            sb.append(')');
        }
        return sb.toString();
    }

    @Override
    void fastWrite(FastWriter writer) throws IOException {
        if (arguments != null) {
            writer.writeCompound(name, arguments.length);
            for (int i = 0, n = arguments.length; i < n; i++) {
                arguments[i].fastWrite(writer);
            }
        } else {
            throw new IllegalStateException("not a compound term: " + toString());
        }
    }

    @Override
    PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
        if (arguments != null) {
            writer.writeCompound(name, arguments.length);
            return arguments;
        } else {
            throw new IllegalStateException("not a compound term: " + toString());
        }
    }

}
