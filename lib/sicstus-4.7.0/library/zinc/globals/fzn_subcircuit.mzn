include "sicstus.mzn";

predicate fzn_subcircuit(array[int] of var int: x) =
	    subcircuit_sicstus(x, index_set(x));
