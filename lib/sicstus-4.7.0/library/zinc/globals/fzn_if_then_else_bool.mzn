include "fzn_arg_max_bool.mzn";

predicate fzn_if_then_else_bool(array[int] of var bool: c, array[int] of bool: x, var bool: y) =
  let {var index_set(c): i;} in
    fzn_maximum_arg_bool(c, i) /\
    x[i] = y;
