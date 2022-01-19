/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

/**
 * Objects implementing this interface are used for holding a query-reference
 * which is used to find multiple solutions to a query.
 * 
 * @see Prolog#openPrologQuery
 *
 */
public interface Query {
    /**
     * Closes a query. All remaining solutions to the query are discarded, and the
     * state of the Prolog engine will be restored to the state it was in when
     * {@link Prolog#openPrologQuery openPrologQuery} was called. If the query is
     * not the most recently opened, all still open queries opened after this query
     * will be closed as well.
     * <p>
     * A {@link SPQuery#close closed} or {@link SPQuery#cut cut} query is
     * invalidated and most operations on it will throw an exception. In particular,
     * you should not call <code>close</code>, <code>cut</code> or
     * <code>nextSolution</code> on an invalidated query.
     * <p>
     * Invalidates all Term objects created since this query was created. If you
     * need to keep data created by the query (i.e. data referred to by {@link Term}
     * objects), you need to convert it to Java data types before calling this
     * function.
     * 
     * @throws NoSuchMethodException
     *             Undocumented
     * @throws InterruptedException
     *             the thread was interrupted
     * @throws Exception
     *             something went wrong
     **/
    void close() throws NoSuchMethodException, InterruptedException, Exception;

    /**
     * Discards choices made since this query object was created, like the goal
     * <code>!</code>.
     * <p>
     * If the query is not the most recently opened, all still open queries opened
     * after this query will be cut as well.
     * <p>
     * A {@link Query#close closed} or {@link Query#cut cut} query is invalidated
     * and most operations on it will throw an exception. In particular, you should
     * not call <code>close</code>, <code>cut</code> or <code>nextSolution</code> on
     * an invalidated query.
     * <p>
     * Invalidates all Term objects created since this query was created. If you
     * need to keep data created by the query (i.e. data referred to by {@link Term
     * Term} objects), you need to convert it to Java data types before calling this
     * function.
     * 
     * @throws NoSuchMethodException
     *             Undocumented
     * @throws InterruptedException
     *             the thread was interrupted
     * @throws Exception
     *             something went wrong
     **/
    void cut() throws NoSuchMethodException, InterruptedException, Exception;

    /**
     * Gets the next solution for the query.
     * 
     * Returns <code>false</code> when there are no more solutions. When no more
     * solutions are needed, the query must be closed using the method
     * <code>close()</code>.
     * <p>
     * Multiple queries can be open at the same time, but the calls to nextSolution
     * must be nested, i.e. refer to the most recently opened query. Invalidates all
     * <code>Term</code> objects created since this query was created
     * <p>
     * Invalidates all SPTerm objects created since this query was created. If you
     * need to keep data created by the previous call to <code>nextSolution</code>
     * to this query (i.e. data referred to by {@link Term Term} objects), you need
     * to convert it to Java data types before calling this function.
     * 
     *
     * @see Prolog#openPrologQuery
     * @see Query#close
     * @see Query#cut
     * 
     * @return False if there are no more solutions, True otherwise.
     * @throws NoSuchMethodException
     *             Undocumented
     * @throws InterruptedException
     *             the thread was interrupted
     * @throws Exception
     *             something went wrong
     */
    boolean nextSolution() throws NoSuchMethodException, InterruptedException, Exception;
}
