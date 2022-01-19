/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;

/**
 * Representation of a Prolog list cells, i.e. a compound term with the functor
 * {@code ./2}.
 */
class PBListCell extends PBCompound {
    private volatile String iCachedString;
    private volatile Boolean iCachedIsTrueString;

    PBListCell(PBTerm head, PBTerm tail) {
        this(new PBTerm[] { head, tail });
    }

    PBListCell(PBTerm[] terms) {
        super(".", terms);
    }

    @Override
    public boolean isListCell() {
        return true;
    }

    // There is no Character.isDefined(int) in .NET
    private boolean isUnicode(int c) {
        return ((c < 0xD800 || c > 0xDFFF) && c <= 0x10FFFD);
    }

    // There is no StringBuilder.appendCodePoint(int) in .NET
    private void appendCodePoint(StringBuilder sb, int c) {
        if (c < 0xFFFF) {
            sb.append((char) c);
        } else {
            int c1 = c - 0x10000;
            int c1h = c1 >> 10; // higher 10 bits of c1
            int c1l = (c1 << 10) >> 10; // lower 10 bits of c1
            int u1 = 0xD800;
            int u2 = 0xDC00;
            u1 = u1 | c1h;
            u2 = u2 | c1l;
            sb.append((char) u1);
            sb.append((char) u2);
        }
    }

    @Override
    public boolean isString() {
        String cachedString = iCachedString;

        if (cachedString != null) {
            return true;
        } else {
            return isStringSlowPath();
        }
    }

    private boolean isStringSlowPath() {
        try {
            getString(); // for side-effect
            return true;
        } catch (@SuppressWarnings("unused") IllegalStateException e) {
            return false;
        }
    }

    /**
     * Returns {@code true} if the receiver is a proper list and all of its elements
     * are character codes. Returns <{@code false} otherwise.
     */
    private boolean isTrueString() {
        Boolean cachedIsTrueString = iCachedIsTrueString;

        if (cachedIsTrueString != null) {
            return cachedIsTrueString.booleanValue();
        }
        return isTrueStringSlowPath();
    }

    private boolean isTrueStringSlowPath() {

        PBTerm tail = this;
        while (tail.isListCell()) {
            PBTerm head = tail.getArgument(1);

            if (!(head.isInteger() && isUnicode((int) head.intValue()))) {
                break;
            }
            tail = tail.getArgument(2);
        }

        boolean trueString = tail.isEmptyList();

        // No need to protect this from multiple writes.
        iCachedIsTrueString = Boolean.valueOf(trueString);

        return trueString;
    }

    @Override
    public String getString() {
        String cachedString = iCachedString;
        if (cachedString != null) {
            return cachedString;
        }
        return getStringSlowPath();
    }

    private String getStringSlowPath() {
        String builtString = buildString().toString();

        // Double-checked-locking idiom
        synchronized (this) {
            String cachedString = iCachedString;

            if (cachedString == null) {
                cachedString = builtString;
                iCachedString = cachedString;
            }

            return cachedString;
        }
    }

    private String buildString() {
        StringBuilder builder = new StringBuilder();
        PBListCell pbt = this;
        PBTerm h;
        do {
            h = pbt.arguments[0];
            if (h.isAtom() && h.name.length() == 1) { // Is head a one char atom?
                builder.append(((PBAtom) h).getName());
            } else if (h.isInteger()) { // Is head an integer?
                // *** FIXME: Perhaps check if this cast truncates.
                int c = (int) h.intValue();
                if (!isUnicode(c)) {
                    throw new IllegalStateException("not a list of characters");
                }
                appendCodePoint(builder, c);
            } else {
                throw new IllegalStateException("not a list of characters");
            }
            PBTerm t = pbt.arguments[1]; // tail()
            if (t == PBTerm.NIL) {
                return builder.toString();
            } else {
                if (t.isListCell()) {
                    pbt = (PBListCell) t;
                } else {
                    throw new IllegalStateException("not a list of characters");
                }
            }
        } while (true);
    }

    /**
     * Returns the head of this list cell.
     */
    @Override
    public PBTerm head() {
        return arguments[0];
    }

    /**
     * Returns the tail of this list cell.
     */
    @Override
    public PBTerm tail() {
        return arguments[1];
    }

    /**
     * Returns the length of this list cell.
     */
    @Override
    public int length() {
        // [PM] 4.1.3 non-recursive version
        int len = 0;
        PBTerm tmp;
        for (tmp = this; tmp.isListCell(); tmp = tmp.tail()) {
            len++;
        }
        // [PM] 4.1.3 "tmp.length()", for verifying properness. Expect tmp to be a PBNil
        // here, i.e. tmp.length()==0.
        len += tmp.length();
        return len;
    }

    @Override
    String toPrologString() {
        StringBuilder sb = new StringBuilder().append('[');
        sb.append(arguments[0].toPrologString());
        PBTerm t = arguments[1];
        while (!t.isEmptyList()) {
            sb.append(',');
            sb.append(t.head().toPrologString());
            t = t.tail();
        }
        sb.append(']');
        return sb.toString();
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder().append('[');
        sb.append(arguments[0].toString());
        PBTerm t = arguments[1];
        while (!t.isEmptyList()) {
            sb.append(',');
            sb.append(t.head().toString());
            t = t.tail();
        }
        sb.append(']');
        return sb.toString();
    }

    @Override
    void fastWrite(FastWriter writer) throws IOException {
        if (isTrueString()) {
            writer.writeString(getString());
        } else {
            super.fastWrite(writer);
        }
    }

    @Override
    PBTerm[] fastWritePrefix(FastWriter writer) throws IOException {
        if (isTrueString()) {
            writer.writeString(getString());
            return PBTerm.NO_TERMS;
        } else {
            return super.fastWritePrefix(writer);
        }
    }

}
