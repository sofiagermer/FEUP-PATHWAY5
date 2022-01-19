include "sicstus.mzn";

predicate fzn_minimum_arg_int(array[int] of var int: x, var int: i) =
    minimum_arg_sicstus(x, i, min(index_set(x)));
