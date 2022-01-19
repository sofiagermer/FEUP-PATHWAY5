/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.prologbeans;

/**
 * The representation of an answer from the Prolog server.
 * <p>
 * The {@code QueryAnswer} is returned by {@link PrologSession} in response to a
 * query and contains variable bindings, errors, and success/failure
 * information. It also contains the variable bindings specified in the query.
 * <p>
 * <b>Note that this implementation is not synchronized.</b>
 */
public class QueryAnswer extends Bindings {
    /**
     * This term is on one of these forms:
     * <p>
     * - A list of the form {@code ['='(VariableNameAsAtom,Value), ...]} (variable
     * bindings).
     * <p>
     * - The atom {@code no} (the prolog responded with 'no').
     * <p>
     * - The compound term {@code error(ErrorReason)} (an error occurred).
     *
     */
    private final PBTerm iAnswer;
    private final boolean iHasValues;

    private boolean bound = false;

    /**
     * Creates a new <code>QueryAnswer</code> instance with the specified
     * information.
     *
     * @param answer
     *            a {@link PBTerm} value representing the Prolog response
     * @param bindings
     *            the variable bindings for the query to which this is an answer
     */
    public QueryAnswer(PBTerm answer, Bindings bindings) {
        super(bindings);
        iAnswer = answer;
        iHasValues = answer.isListCell();
    }

    /**
     * Returns the value of the specified variable or <code>null</code> if the
     * variable is not bound.
     *
     * @param variable
     *            the name of the variable
     * @return the value of the variable as a {@link PBTerm} or <code>null</code> if
     *         the variable is not bound
     */
    @Override
    public PBTerm getValue(String variable) {
        if (!bound) {
            bind();
            bound = true;
        }
        return super.getValue(variable);
    }

    /**
     * Transfer bindings from the answer bindings list to the internal bindings map.
     * <p>
     * Must be called at most once.
     */
    private void bind() {
        if (iHasValues) {
            // [PM] 4.1.3 for type checking only. Throws an exception if not a proper list.
            iAnswer.length();

            // copy all the received bindings into Bindings
            for (PBTerm list = iAnswer; list.isListCell(); list = list.getArgument(2)) {
                PBTerm bindTerm = list.getArgument(1);
                if (bindTerm.getName().equals("=")) {
                    bind(bindTerm.getArgument(1).getName(), bindTerm.getArgument(2));
                }
            }
        }
    }

    /**
     * Returns <code>true</code> if the query failed (i.e. the Prolog responded with
     * 'no') and <code>false</code> otherwise.
     *
     * @return whether the query failed
     */
    public boolean queryFailed() {
        return !iHasValues && iAnswer.getName().equals("no");
    }

    /**
     * Returns <code>true</code> if an error occurred while processing the query and
     * <code>false</code> otherwise.
     *
     * @return whether the query threw an exception
     */
    public boolean isError() {
        return !iHasValues && iAnswer.getName().equals("error");
    }

    /**
     * Returns the error reason or <code>null</code> if an error has not occurred or
     * if no error reason is known.
     *
     * @return the error, or null, if none
     */
    public String getError() {
        if (iAnswer.getName().equals("error")) {
            return iAnswer.getArgument(1).toString();
        }
        return null;
    }

    /**
     * Returns a string description of this answer.
     */
    @Override
    public String toString() {
        return iAnswer.toString();
    }

} // QueryAnswer
