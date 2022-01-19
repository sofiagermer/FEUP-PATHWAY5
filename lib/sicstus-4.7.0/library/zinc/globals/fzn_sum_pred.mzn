include "sicstus.mzn";

predicate fzn_sum_pred(var int: i, array[int] of set of int: sets,
                       array[int] of int: cs, var int: s) =
    sum_pred_sicstus(i, sets, cs, s, min(index_set(cs)));
