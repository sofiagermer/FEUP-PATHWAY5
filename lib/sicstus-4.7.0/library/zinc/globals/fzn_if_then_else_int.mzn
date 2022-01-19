include "fzn_arg_max_bool.mzn";

predicate fzn_if_then_else_int(array[int] of var bool: c, array[int] of int: x, var int: y) =
  let {int: a = min(index_set(c));
       int: b = max(index_set(c));
  } in if a+1 = b then
         let {var bool: elsecase = (c[b] /\ not c[a]);
	 } in y = elsecase * (x[b] - x[a]) + x[a]
       else
	 let {var index_set(c): i;} in
	   fzn_maximum_arg_bool(c, i) /\
	   x[i] = y
       endif
  ;

