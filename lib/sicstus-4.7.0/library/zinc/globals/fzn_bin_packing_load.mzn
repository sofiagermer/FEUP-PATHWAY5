include "sicstus.mzn";

predicate fzn_bin_packing_load(array[int] of var int: load,
                           array[int] of var int: bin,
                           array[int] of int: w) =
    bin_packing_load_sicstus(load, bin, w, min(index_set(load)));
