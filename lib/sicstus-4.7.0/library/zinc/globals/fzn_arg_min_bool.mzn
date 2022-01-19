include "sicstus.mzn";

predicate fzn_minimum_arg_bool(array[int] of var bool: x, var int: i) =
    minimum_arg_sicstus(x, i, min(index_set(x)));
