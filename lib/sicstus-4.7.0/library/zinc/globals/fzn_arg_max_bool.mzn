include "sicstus.mzn";

predicate fzn_maximum_arg_bool(array[int] of var bool: x, var int: i) =
    maximum_arg_sicstus(x, i, min(index_set(x)));
