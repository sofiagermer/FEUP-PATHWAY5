/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

import java.util.HashMap;

/**
 * <strong>Deprecated</strong> Instead use module and predicate name as strings
 * directly or specify the goal as a string.
 *
 * <p>
 * You should not inherit from {@code SPPredicate}.
 *
 * @see SICStus#query
 * @see SICStus#queryCutFail
 * @see SICStus#openQuery
 * @deprecated Instead use module and predicate name as strings directly or
 *             specify the goal as a string.
 */
@Deprecated
public class SPPredicate {
    @SuppressWarnings("unused")
    private SICStus sp;
    private final String module;
    private final String name;
    private final int arity;

    /**
     * 
     * <strong>Deprecated</strong> Creates a predicate reference. Instead use module
     * and predicate name as strings directly or specify the goal as a string.
     * 
     * @param sp
     *            A reference to the SICStus object to which this predicate belongs
     *            to.
     * @param name
     *            The predicate's name.
     * @param arity
     *            The arity of the predicate (in [0..255])
     * @param module
     *            The module to use when calling the predicate. When calling the
     *            predicate the module (M) specified is used as if the call was
     *            call(M:NAME(ARGS)). This means that the module specified will
     *            affect calls to meta predicates and goal expansion. For backward
     *            compatibility module can be null or "" to signify the type-in
     *            module but this use is deprecated (and slow).
     * 
     * @throws SPException
     *             The predicate reference could not be created. The usual cause for
     *             this is that the predicate is not defined (although detecting
     *             undefined predicates in this manner is <strong>not</strong>
     *             guaranteed and will not happen by default as of 3.8.5).
     * @deprecated Instead use module and predicate name as strings directly or
     *             specify the goal as a string.
     */
    @Deprecated
    public SPPredicate(SICStus sp, String name, int arity, String module) throws SPException {
        this.sp = sp;
        if (!(0 <= arity && arity <= 255))
            throw new SPException(sp, "Predicate arity not in [0..255]");
        this.arity = arity;
        if (name == null)
            throw new NullPointerException("Predicate name is null");
        this.name = name;

        if (module == null || module.isEmpty()) { // use type-in module
            synchronized (sp) {
                SPQuery context = sp.openContext();
                try {
                    HashMap<String, Term> map = new HashMap<>();
                    sp.query("prolog_flag(typein_module, X).", map);
                    module = ((SPTerm) map.get("X")).getString();
                } finally {
                    context.close();
                }
            }
        }
        this.module = module;
    }

    /**
     * <strong>Deprecated</strong> Creates a predicate reference. Instead use module
     * and predicate name as strings directly or specify the goal as a string.
     *
     * @param sp
     *            A reference to the SICStus object to which this predicate belongs
     *            to.
     * @param name
     *            The predicate's name.
     * @param arity
     *            The arity of the predicate (in [0..255])
     * @param module
     *            The module to use when calling the predicate. When calling the
     *            predicate the module (M) specified is used as if the call was
     *            call(M:NAME(ARGS)). This means that the module specified will
     *            affect calls to meta predicates and goal expansion.
     * @throws SPException
     *             if an Prolog exception was thrown
     * @deprecated Instead use module and predicate name as strings directly or
     *             specify the goal as a string.
     */
    @Deprecated
    public SPPredicate(SICStus sp, SPCanonicalAtom name, int arity, SPCanonicalAtom module) throws SPException {
        this(sp, name.toString(), arity, ((module == null) ? null : module.toString()));
    }

    public String getModule() {
        return module;
    }

    public String getName() {
        return name;
    }

    public int getArity() {
        return arity;
    }
}

/*-
 * [PM] Keep the original indentation style
 * Local variables:
 * indent-tabs-mode: nil
 * c-basic-offset: 4
 * end:
 **/
