include "sicstus.mzn";

predicate fzn_geost_smallest_bb(
                      int        : k           ,
    array[int,int] of int        : rect_size   ,
    array[int,int] of int        : rect_offset ,
    array[int    ] of set of int : shape       , 
    array[int,int] of var int    : x           ,
    array[int    ] of var int    : kind        ,
    array[int    ] of var int    : l           ,
    array[int    ] of var int    : u
) = 
        % A few useful definitions
        let { 
            set of int: DIMS    = 1..k;
	    set of int: SBOXES  = index_set_1of2(rect_size);
            set of int: SHAPES  = 1..length(shape);
            set of int: OBJECTS = index_set(kind); 
	    array[int] of 0..1: shape01 = [b in shape[s] | s in SHAPES, b in SBOXES]
	} in
          geost_sicstus(k, array1d(rect_size), array1d(rect_offset), shape01, array1d(x), kind, l, u)
	;

