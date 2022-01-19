/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map.Entry;

/**
 * {@link Bindings} handles the variable bindings in the communication with the
 * prolog server. Using variable bindings ensures that the values are properly
 * quoted when sent to the prolog server.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 */
public class Bindings {

    private final HashMap<String, PBTerm> iBindings;

    /**
     * Creates a new {@link Bindings} instance with no variable bindings.
     */
    public Bindings() {
        this(new HashMap<>());
    }

    /**
     * Creates a new {@link Bindings} instance and copies all existing variable
     * bindings from the specified bindings.
     *
     * @param bindings
     *            the variable bindings to copy
     */
    public Bindings(Bindings bindings) {
        this((bindings != null ? new HashMap<>(bindings.iBindings) : new HashMap<>()));
    }

    private Bindings(HashMap<String, PBTerm> bindings) {
        iBindings = bindings;
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param intvalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, int intvalue) {
        return bind(name, new PBInteger(intvalue));
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param longvalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, long longvalue) {
        return bind(name, new PBInteger(longvalue));
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param floatvalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, float floatvalue) {
        return bind(name, new PBFloat(floatvalue));
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param doublevalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, double doublevalue) {
        return bind(name, new PBFloat(doublevalue));
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param stringvalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, String stringvalue) {
        return bind(name, createListFromString(stringvalue));
    }

    // [PD] 4.0.0 Non-recursive version.
    private PBTerm createListFromString(String str) {
        if (str.length() == 0) {
            return PBTerm.NIL;
        } else {
            PBListCell pblc1 = new PBListCell(null, PBTerm.NIL);
            PBTerm pblc2 = pblc1;
            PBTerm pblct = pblc2;
            for (int i = 0; i < str.length(); i++) {
                pblct = pblc2;
                ((PBListCell) pblc2).arguments[0] = new PBInteger(str.charAt(i));
                PBListCell pblc3 = new PBListCell(null, PBTerm.NIL);
                ((PBListCell) pblc2).arguments[1] = pblc3;
                pblc2 = pblc3;
            }
            ((PBListCell) pblct).arguments[1] = PBTerm.NIL;
            return pblc1;
        }
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'.
     *
     * @param name
     *            a prolog variable name
     * @param termvalue
     *            the value to bind to the variable
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bind(String name, PBTerm termvalue) {
        checkVar(name);
        iBindings.put(name, termvalue);
        return this;
    }

    /**
     * Adds the specified variable binding. The variable name must start with an
     * upper case letter or '_'. The value will be bound as an atom.
     *
     * @param name
     *            a prolog variable name
     * @param atomvalue
     *            the value to bind to the variable as an atom
     * @return a reference to this {@link Bindings} object
     * @throws IllegalArgumentException
     *             if the name is not a valid prolog variable name
     */
    public Bindings bindAtom(String name, String atomvalue) {
        // [PM] 4.2.1 guard against "[]", so do not use new PBAtom(atomvalue) directly.
        return bind(name, PBTerm.makeAtom(atomvalue));
    }

    private void checkVar(String name) {
        if (name.length() > 0) {
            char c = name.charAt(0);

            if (c == '_' || Character.isUpperCase(c)) {
                return;
            }
        }

        throw new IllegalArgumentException("Variable names must start with uppercase letter or '_' : " + name);
    }

    /**
     * Returns the value for the specified variable or <code>null</code> if the
     * variable is not bound.
     *
     * @param name
     *            the name of the variable
     * @return the value of the variable as a {@link PBTerm PBTerm} or
     *         <code>null</code> if the variable is not bound
     */
    public PBTerm getValue(String name) {
        return iBindings.get(name);
    }

    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        builder.append('[');
        String prefix = "";

        for (Entry<String, PBTerm> e : iBindings.entrySet()) {
            String key = e.getKey();
            PBTerm value = e.getValue();
            builder.append(prefix).append(key).append('=').append(value.toPrologString());
            prefix = ",";
        }

        builder.append(']');
        return builder.toString();
    }

    void fastWrite(FastWriter writer) throws IOException {
        for (Entry<String, PBTerm> e : iBindings.entrySet()) {
            String key = e.getKey();
            PBTerm value = e.getValue();
            writer.writeList();

            // =(key,value)
            writer.writeCompound("=", 2);
            writer.writeAtom(key);
            writer.writeTerm(value);
        }
        writer.writeNIL();
    }

}
