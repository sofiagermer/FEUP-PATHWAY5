include "sicstus.mzn";

predicate fzn_table_int(array[int] of var int: x, array[int, int] of int: t2) =
        let {
            set of int: is = index_set_1of2(t2),
            set of int: js = index_set_2of2(t2),
            int : l = card(is) * card(js),
            array [1..l] of int : t = [t2[i,j] | i in is, j in js]
        }
	in table_int_sicstus(x, t)
	;
