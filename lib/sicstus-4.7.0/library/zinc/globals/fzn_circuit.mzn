include "sicstus.mzn";

predicate fzn_circuit(array[int] of var int: x) =
	    circuit_sicstus(x, index_set(x));
