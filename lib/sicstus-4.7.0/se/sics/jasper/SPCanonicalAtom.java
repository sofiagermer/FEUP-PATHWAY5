/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * The SPCanonicalAtom class encapsulates the canonical representation of a
 * Prolog atom, which is usually a 32 or 64-bit integer. In previous versions of
 * Jasper, atoms were passed around as Java long types. However, this prevented
 * interaction with the atom garbage collector. By using this class instead,
 * atoms only referenced canonically from Java are ensured not to be garbage
 * collected.
 * 
 * <p>
 * You should not inherit from {@code SPCanonicalAtom}.
 *
 * @see SPTerm#getCanonicalAtom
 */
public class SPCanonicalAtom {
    private final SICStus sp;
    private final String string;

    // The sp arguments below are not used for now, but may be useful in
    // the future if multiple atom databases (i.e. multiple Prolog engines)
    // become possible.

    boolean isValid() {
        return true;
    }

    boolean isValid(SICStus sp) {
        return isValid() && (sp == this.sp);
    }

    /**
     * Throws an exception of the atom is not valid.
     * 
     * @see SPCanonicalAtom#isValid
     */
    void CheckValid() throws IllegalTermException {
        if (!isValid()) {
            throw new IllegalTermException(sp); // NOTE: need a better exception
        }
    }

    void CheckValid(SICStus sp) throws IllegalTermException {
        if (!isValid(sp)) {
            throw new IllegalTermException(this.sp); // NOTE: need a better exception
        }
    }

    /**
     * Creates a canonical atom from a string.
     * 
     * @param sp
     *            the Prolog where the atom should be created
     * @param string
     *            the name of the atom
     * @throws AtomRegisterFailure
     *             if the atom could not be registered, for any reason
     */
    public SPCanonicalAtom(SICStus sp, String string) throws AtomRegisterFailure {
        // [PM] 4.3.1 FIXME: This looks strange
        if (string == null)
            throw new AtomRegisterFailure(sp,
                    "could not register atom \"" + (string == null ? "?null?" : string) + "\"");
        this.sp = sp;
        this.string = string;
    }

    /** Returns this canonical atom as a string */
    @Override
    public String toString() {
        return string;
    }

    /* [PM] 3.9 The Prolog side string, currently the same as toString() */
    /** Returns this canonical atom as a string */
    String getString() {
        return string;
    }

    /**
     * Returns true iff the atoms have the same canonical representation.
     * 
     * @param cAtom
     *            the atom to check against
     * @return whether the argument represents the same atom as the receiver.
     * @throws IllegalTermException
     *             an illegal term was detected
     */
    public boolean isSameAtom(SPCanonicalAtom cAtom) throws IllegalTermException {
        CheckValid();
        cAtom.CheckValid(sp);
        return this.string.equals(cAtom.string);
    }

}

/*-
 * [PM] Keep the original indentation style
 * Local variables:
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * end:
 */
