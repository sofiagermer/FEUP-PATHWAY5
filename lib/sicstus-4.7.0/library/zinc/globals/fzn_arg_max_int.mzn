include "sicstus.mzn";

predicate fzn_maximum_arg_int(array[int] of var int: x, var int: i) =
    maximum_arg_sicstus(x, i, min(index_set(x)));


