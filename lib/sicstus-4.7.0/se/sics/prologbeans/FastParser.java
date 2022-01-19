/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.HashMap;

/*-
 * <code>FastParser</code>
 *
 * Documentation of the fast_write/read "protocol"
 *
 * Each "string" looks like:
 *
 * [PM] 3.10.2 Updated specification
 *
 * D TYPE [type-specific data]
 *
 * D -> start of fastrw Term (D is also the version number)
 *
 * TYPE, one byte for type:
 *  I - integer - argument is a arbitrary long, nonempty string of
 *     ASCII decimal digits, prefixed by a minus sign if negative
 *     There should be no plus sign prefixed. There is no size restriction.
 *  F - float - argument is an ASCII string of digits with a decimal
 *     point preceeded by at least one digit, optionally using
 *     E-notation (capital E) with exponent optionally prefixed by
 *     minus sign. The floating point number is prefixed by minus if
 *     negative
 *  A - atom - argument is an ATOMNAME (*)
 *     Make UTF-8 explicit (QP loses here). (Luckily this should be exactly
 *     the Java String.getbytes bytes.)
 *  _ - variable (followed by DATA which is a string of digits). The
 *      variables are numbered 0..n in depth first traversal
 *      increasing argument order. This is the change in version 'D'
 *      compared to version 'C'. The variable numbering is strictly defined.
 *
 * S ATOMNAME n - compound with n terms [followed by n terms] - n is
 *      an (unsigned) byte
 * " - a list prefix consisting of only integers in [1..255] (*)
 *     the sequence is terminated by zero byte followed by the tail
 *     term. Note that this is just a compact representation of
 *     nested compound terms with functor ./2.
 *     Example "ab", i.e., .(97, .(98, [])) becomes (using C
 *     char-constant notation) '"' 'a' 'b' '\0' ']'; [x,97,98|y]
 *     becomes '[' 'A' 'x' '\0' '"' 'a' 'b' '\0' 'A' 'y' '\0'
 > Clarified that this encoding is used for any "list"-prefix with
 > suitable elements. In particular it is not always followed by ']'.
 > Also note that the elements of this kind of list should *not* be
 > treated by the reader as individual bytes of an UTF-8 encoded
 > string. If a list is to be treated as a string then each element
 > should be treated as a single UNICODE character, this holds for
 > lists transmitted using any of the three ('"', '[' or 'S' '.' '\0'
 > '\2') possible serialization-formats.
 >
 * [ - a list cell, i.e, a shortcut for 'S' '.' '\0' '\2' (*)
 * ] - empty list, i.e, a shortcut for 'A' '[' ']' '\0' (*)
 *
 * DATA - a byte sequence terminated by '\0'
 *     NOTE: As of verson 'D' the numbering must be sequential from
 *           _0.._n, the prolog side fast_read will most likely
 *           crash if * this invariant is not maintained when
 *           sending data to * prolog.
 * ATOMNAME is DATA where the bytes make up the UTF-8 name of the
 *     atom.
 *
 *   (*) These could be optional in the writer since there are longer
 *       equivalent forms from which the reader could produce the same term.
 */

class FastParser {

    static final byte VERSION = (byte) 'D';
    static final byte INTEGER = (byte) 'I';
    static final byte FLOAT = (byte) 'F';
    static final byte ATOM = (byte) 'A';
    static final byte COMPOUND = (byte) 'S';
    static final byte VARIABLE = (byte) '_';
    static final byte STRING = (byte) '"';
    static final byte LIST = (byte) '[';
    static final byte NIL = (byte) ']';

    public FastParser() {

    }

    public PBTerm parseProlog(InputStream stream) throws IOException {
        int c;

        if ((c = stream.read()) == VERSION) {
            return parseTermWithStack(stream, new HashMap<>());
        } else {
            // [PD] 4.0.5 Stopgap solution until we implement a well designed exception
            // strategy in Prologbeans.
            throw new IOException("Not a fast prolog expression" + c);
        }
    }

    private static final boolean logging = false;

    static private class Work {
        // Next read term should go into args[i]
        int i;
        final PBTerm[] args;
        final Work next;

        public Work(PBTerm[] args, Work next) {
            // assert args != null;
            // assert args.length > 0;
            this.args = args;
            this.i = 0;
            this.next = next;
        }

        /**
         * Update top of stack. Return the resulting stack, i.e. either 'this' or
         * this.next.
         */
        Work bump(PBTerm term) {
            // assert i < args.length;
            // assert term != null;
            args[i] = term;
            i++;
            if (i < args.length) {
                if (logging) {
                    System.out.println("bump() returning this: " + this);
                }
                return this;
            }
            if (logging) {
                System.out.println("bump() returning next: " + next);
            }
            return next;
        }

        /*
         * bump the term onto the stack and then push terms on the resulting stack.
         * Never null.
         */
        public Work bump(PBTerm term, PBTerm[] terms) {
            // Note that bump(term) may return null. This is why we do not use
            // something like bump(term).push(terms).
            Work tmp = new Work(terms, bump(term));
            if (logging) {
                System.out.println("bump() returning new: " + tmp);
            }
            return tmp;
        }

        @Override
        public String toString() {
            return "[" + super.toString() + " at " + i + "/" + args.length + "]";
        }
    }

    /*
     * [PM] 4.1.3 Manage an explicit stack to avoid running out of Java stack (SPRM
     * 11909)
     */
    private PBTerm parseTermWithStack(InputStream stream, HashMap<String, PBVariable> variableTable)
            throws IOException, NumberFormatException {
        final Work outer = new Work(new PBTerm[1], null);
        Work stack = outer;
        do {
            int chr = stream.read();

            PBTerm term;

            switch (chr) {
            case INTEGER: {
                String val = getString(stream);
                try {
                    term = new PBInteger(Long.parseLong(val, 10), val);
                } catch (@SuppressWarnings("unused") NumberFormatException e) {
                    term = new PBBignum(new BigInteger(val, 10), val);
                }
                if (logging) {
                    System.out.println("bump() INTEGER " + val);
                }
                stack = stack.bump(term);
            }
                break;
            case FLOAT: {
                String val = getString(stream);
                term = new PBFloat(Double.parseDouble(val), val);
                if (logging) {
                    System.out.println("bump() FLOAT " + val);
                }
                stack = stack.bump(term);
            }
                break;
            case ATOM: {
                String val = getString(stream);
                term = new PBAtom(val);
                if (logging) {
                    System.out.println("bump() ATOM " + val);
                }
                stack = stack.bump(term);
            }
                break;
            case VARIABLE: {
                String varName = getString(stream);
                PBVariable var = variableTable.get(varName);
                if (var == null) {
                    var = new PBVariable(varName);
                    variableTable.put(varName, var);
                }
                term = var;
                if (logging) {
                    System.out.println("bump() VARIABLE " + varName);
                }
                stack = stack.bump(term);
            }
                break;
            case STRING: {
                byte[] val = getBytes(stream);
                // FIXME: This still runs the risk of blowing the stack
                PBTerm t = parseTermWithStack(stream, variableTable);

                // Note that this builds the list from the end.
                for (int i = val.length - 1; i >= 0; i--) {
                    // [PM] 4.3 anding to convert from signed byte to unsigned 0..255 integer
                    t = new PBListCell(new PBInteger(val[i] & 0xFF), t);
                }
                term = t;
                if (logging) {
                    System.out.println("bump() STRING " + val);
                }
                stack = stack.bump(term);
            }
                break;
            case LIST: {
                int noTerms = 2;
                PBTerm[] terms = new PBTerm[noTerms];
                term = new PBListCell(terms);
                if (logging) {
                    System.out.println("bump() LIST ./2");
                }
                stack = stack.bump(term, terms);
            }
                break;
            case NIL: {
                term = PBTerm.NIL;
                if (logging) {
                    System.out.println("bump() NIL");
                }
                stack = stack.bump(term);
            }
                break;
            case COMPOUND: {
                String val = getString(stream);
                int noTerms = stream.read();
                PBTerm[] terms = new PBTerm[noTerms];
                // [PM] 4.1.3 ensure we get a PBListCell even if writer sent it
                // as plain compound
                term = PBTerm.makeTerm(val, terms);

                if (logging) {
                    System.out.println("bump() COMPOUND " + val + "/" + noTerms);
                }
                stack = stack.bump(term, terms);
            }
                break;
            default:
                throw new IOException("Parse error: illegal character " + (char) chr);
            }
        } while (stack != null);

        if (logging) {
            System.out.println("parseTermWithStack returning " + outer.args[0]);
        }
        return outer.args[0];
    }

    private String getString(InputStream stream) throws IOException {
        // In Java 10 we should use byteBuff.toString(StandardCharsets.UTF_8);
        return getByteBuf(stream).toString("UTF8");
    }

    private byte[] getBytes(InputStream stream) throws IOException {
        return getByteBuf(stream).toByteArray();
    }

    private ByteArrayOutputStream getByteBuf(InputStream stream) throws IOException {
        ByteArrayOutputStream byteBuff = new ByteArrayOutputStream();
        int b; // [-1,0..255]
        while ((b = stream.read()) > 0) {
            byteBuff.write(b);
        }
        if (b == 0) {
            return byteBuff;
        }

        throw new EOFException();
    }

}
