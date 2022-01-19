include "sicstus.mzn";

predicate fzn_cost_regular(array[int] of var int: x, int: Q, int: S,
                           array[int,int] of int: d, int: q0, set of int: F,
                           array[int,int] of int: c, var int: C) =
    let {
            set of int: row_indexes = index_set_1of2(d),
            set of int: column_indexes = index_set_2of2(d),
	    int : n_rows = card(row_indexes),
	    int : n_columns = card(column_indexes),
            int : nn = n_rows * n_columns,
            array [1..nn] of int : dd = [d[i,j] | i in row_indexes, 
                                                  j in column_indexes],
            array [1..nn] of int : cc = [c[i,j] | i in row_indexes, 
                                                  j in column_indexes],
        } in cost_regular_sicstus(x, Q, S, dd, q0, F, cc, C);

