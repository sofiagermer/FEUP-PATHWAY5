include "sicstus.mzn";

predicate fzn_bin_packing_capa(array[int] of int: c,
                               array[int] of var int: bin,
                               array[int] of int: w) =
    bin_packing_capa_sicstus(c, bin, w, min(index_set(c)));
