include "fzn_inverse.mzn";

predicate fzn_symmetric_all_different(array[int] of var int:x) =
    fzn_inverse(x, x);
