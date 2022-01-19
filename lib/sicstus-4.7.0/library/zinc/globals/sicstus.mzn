predicate alldiff_0_sicstus(array[int] of var int: x, int: u);

predicate arg_sort_int_sicstus(array[int] of var int: x, array[int] of var int: p, int: base);

predicate bin_packing_capa_sicstus(array[int] of int: c,
                           	   array[int] of var int: bin,
                           	   array[int] of int: w,
			   	   int: min_index);

predicate bin_packing_load_sicstus(array[int] of var int: l,
                                   array[int] of var int: bin,
                                   array[int] of int: w,
			   	   int: min_index);

predicate inverse_sicstus(array[int] of var int: f, array[int] of var int: invf, int: min_index_f, int: min_index_invf);
   
predicate table_bool_sicstus(array[int] of var bool: x, array[int] of bool: t);
predicate table_int_sicstus(array[int] of var int: x, array[int] of int: t);

predicate pairwise_relation_bool_sicstus(array[int] of var bool: x, int: relop);
predicate pairwise_relation_int_sicstus(array[int] of var int: x, int: relop);

predicate lex2_int_sicstus(array[int] of var int: x, int: nrows, int: ncolumns, int: relop);

predicate sum_pred_sicstus(var int: i, array[int] of set of int: sets, array[int] of int: cs, var int: s, int: min_index);

predicate value_precede_sicstus(int: s, int: t, array[int] of var int: x);

predicate value_precede_chain_sicstus(array[int] of int: c, array[int] of var int: x);

predicate regular_sicstus(array[int] of var int: x, int: Q, int: S,
                          array[int] of int: d, int: q0, set of int: F);

predicate cost_regular_sicstus(array[int] of var int: x, int: Q, int: S,
                               array[int] of int: d, int: q0, set of int: F,
			       array[int] of int: c, var int: C);

predicate mdd_sicstus(array[int] of var int: x,
		      array[int] of int: level,
		      array[int] of int: from,
		      array[int] of set of int: label,
		      array[int] of int: to);

predicate cost_mdd_sicstus(array[int] of var int: x,
			   array[int] of int: level,
			   array[int] of int: from,
			   array[int] of set of int: label,
			   array[int] of int: to,
			   array[int] of int: cost,
			   var int: totalcost);

predicate circuit_sicstus(array[int] of var int: x, set of int: y);

predicate subcircuit_sicstus(array[int] of var int: x, set of int: y);

predicate minimum_arg_sicstus(array[int] of var int: x, var int: i, int: base);
predicate maximum_arg_sicstus(array[int] of var int: x, var int: i, int: base);

predicate geost_sicstus(
                      int        : k           ,
    array[int    ] of int        : rect_size   ,
    array[int    ] of int        : rect_offset ,
    array[int    ] of 0..1       : shape01     , 
    array[int    ] of var int    : x           ,
    array[int    ] of var int    : kind        ,
    array[int    ] of var int    : l           ,
    array[int    ] of var int    : u
);

include "seq_precede_chain.mzn";
predicate fzn_seq_precede_chain_int(array[int] of var int: x);

annotation geost_corners;
annotation geost_cumulative;
annotation geost_disjunctive;
annotation geost_dynamic_programming;
annotation geost_longest_hole;
annotation geost_pallet_loading;
annotation geost_parconflict;
annotation geost_polymorphism;
annotation geost_task_intervals;
annotation geost_visavis;
annotation geost_visavis_floating;
annotation geost_visavis_init;
