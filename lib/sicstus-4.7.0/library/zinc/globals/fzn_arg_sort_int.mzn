include "sicstus.mzn";

predicate fzn_arg_sort_int(array[int] of var int: x, array[int] of var int: p) =
    arg_sort_int_sicstus(x, p, min(index_set(x)));
