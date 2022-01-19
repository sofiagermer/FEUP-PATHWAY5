include "sicstus.mzn";

predicate fzn_inverse(array[int] of var int: f, array[int] of var int: invf) = 
    inverse_sicstus(f, invf, min(index_set(f)), min(index_set(invf)));

    
