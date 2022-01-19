/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;

/**
 * Serialize terms in a way that {@code library/fastrw.c} expects.
 */
class FastWriter {
    private static final Charset OUTPUT_CHARSET = StandardCharsets.UTF_8;

    private final OutputStream iOutput;
    private final HashMap<PBVariable, String> iVariableTable;

    private boolean isWritingTerm = false;

    public FastWriter(OutputStream output) {
        iOutput = output;
        iVariableTable = new HashMap<>();
    }

    private void initOutput() throws IOException {
        if (!isWritingTerm) {
            isWritingTerm = true;
            iOutput.write(FastParser.VERSION);
        }
    }

    public void writeCompound(String name, int arguments) throws IOException {
        initOutput();
        iOutput.write(FastParser.COMPOUND);
        iOutput.write(name.getBytes(OUTPUT_CHARSET));
        iOutput.write(0);
        iOutput.write(arguments);
    }

    public void writeList() throws IOException {
        initOutput();
        iOutput.write(FastParser.LIST);
    }

    public void writeNIL() throws IOException {
        initOutput();
        iOutput.write(FastParser.NIL);
    }

    public void writeString(String value) throws IOException {
        initOutput();
        writeCharList(value);
        iOutput.write(FastParser.NIL);
    }

    private void writeCharList(String value) throws IOException {
        boolean in_compact_list = false;
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            if (0 < c && c < 256) {
                if (!in_compact_list) {
                    iOutput.write(FastParser.STRING);
                }
                iOutput.write(c);
                in_compact_list = true;
            } else {
                if (in_compact_list) {
                    iOutput.write(0);
                }
                iOutput.write(FastParser.LIST);
                iOutput.write(FastParser.INTEGER);
                iOutput.write(Integer.toString(c).getBytes());
                iOutput.write(0);
                in_compact_list = false;
            }
        }
        if (in_compact_list) {
            iOutput.write(0);
        }
    }

    public void writeAtom(String value) throws IOException {
        initOutput();
        iOutput.write(FastParser.ATOM);
        iOutput.write(value.getBytes(OUTPUT_CHARSET));
        iOutput.write(0);
    }

    public void writeAtomic(PBAtomic atomic) throws IOException, IllegalStateException {
        initOutput();

        int type = atomic.getType();
        switch (type) {
        case PBAtomic.ATOM:
            iOutput.write(FastParser.ATOM);
            break;
        case PBAtomic.FLOAT:
            iOutput.write(FastParser.FLOAT);
            break;
        case PBAtomic.INTEGER:
            iOutput.write(FastParser.INTEGER);
            break;
        default:
            throw new IllegalStateException("illegal type: " + type);
        }
        iOutput.write(atomic.getName().getBytes(OUTPUT_CHARSET));
        iOutput.write(0);
    }

    public void writeVariable(PBVariable var) throws IOException {
        // frw_read_term() in fastrw.c expects only numbers after the
        // variable prefix. Each new variable is is names using the smallest unused
        // integer, starting from zero.
        String variableName = iVariableTable.get(var);
        if (variableName == null) {
            variableName = Integer.toString(iVariableTable.size());
            iVariableTable.put(var, variableName);
        }
        iOutput.write(FastParser.VARIABLE);
        iOutput.write(variableName.getBytes(OUTPUT_CHARSET));
        iOutput.write(0);
    }

    public void commit() throws IOException {
        iOutput.flush();
        isWritingTerm = false;
        iVariableTable.clear();
    }

    public void close() throws IOException {
        commit();
        iOutput.close();
    }

    static private class WriteWork {
        private int i;
        private final PBTerm[] args;
        private final WriteWork next;

        public WriteWork(PBTerm[] args, WriteWork next) {
            // assert args != null;
            // assert args.length > 0;
            this.i = 0;
            this.args = args;
            this.next = next;
        }

        /**
         * Get next term to process at this level or null. If null is returned then use
         * getNext() to pop this level
         */
        PBTerm bump() {
            if (i < args.length) {
                return args[i++];
            }
            return null;
        }

        public WriteWork getNext() {
            return next;
        }

        public WriteWork push(PBTerm[] args) {
            // assert args != null && args.length > 0;
            return new WriteWork(args, prune());
        }

        /**
         * Pop exhausted stack entries and return the resulting stack (which may be
         * null).
         */
        private WriteWork prune() {
            WriteWork tmp = this;
            while (tmp != null && tmp.i == tmp.args.length) {
                tmp = tmp.getNext();
            }
            return tmp;
        }
    }

    void writeTerm(PBTerm term) throws IOException {
        WriteWork stack = new WriteWork(new PBTerm[] { term }, null);
        do {
            PBTerm t = stack.bump();
            if (t != null) {
                PBTerm[] args = t.fastWritePrefix(this);
                if (args.length > 0) {
                    stack = stack.push(args);
                }
            } else {
                // top-most entry exhausted, pop it.
                stack = stack.prune();
            }
        } while (stack != null);
    }

} // FastWriter
