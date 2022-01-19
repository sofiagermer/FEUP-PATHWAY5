/* Copyright(C) 1997, Swedish Institute of Computer Science */

%   File       : clpfd.pl
%   Author     : Mats Carlsson
%   Updated    : 9 October 2000
%   Purpose    : Finite domains constraint solver

:- module(clpfd, [
	% enumeration
	indomain/1,
	labeling/2,
	solve/2,
	first_bound/2,
	later_bound/2,
	minimize/2,
	minimize/3,
	maximize/2,
	maximize/3,
	/* order_resource/2, */
	% reflection
	fd_var/1,
	fd_min/2,
	fd_max/2,
	fd_size/2,
	fd_set/2,
	fd_dom/2,
	fd_degree/2,
	fd_failures/2,
	fd_set_failures/2,
	fd_statistics/0,
	fd_statistics/2,
	fd_neighbors/2,
	fd_closure/2,
	fd_purge/1,
	% constraints
	domain/3,
	iff/2,					% for compatibility
	in/2,
	in_set/2,
	all_different/1,
	all_different/2,
	all_distinct/1,
	all_distinct/2,
	all_different_except_0/1,
	all_distinct_except_0/1,
	element/3,
	element/2,
	relation/3,		% deprecated
	minimum/2,
	maximum/2,
	nvalue/2,
        geost/2,
        geost/3,
        geost/4,
	circuit/1,
	circuit/2,
	subcircuit/1,
	subcircuit/2,
	assignment/2,
	assignment/3,
	cumulative/1,
	cumulative/2,
        disjoint1/1,
        disjoint1/2,
        disjoint2/1,
        disjoint2/2,
        diffn/1,
        diffn/2,
        case/3,
        case/4,
        table/2,
        table/3,
	cumulatives/2,
	cumulatives/3,
	global_cardinality/2,
	global_cardinality/3,
	count/4,		% deprecated
	sum/3,
	scalar_product/4,
	scalar_product/5,
	scalar_product_reif/5,
	scalar_product_reif/6,
	sorting/3,
	keysorting/2,
	keysorting/3,
	lex_chain/1,
	lex_chain/2,
        automaton/3,
        automaton/8,
        automaton/9,
	regular/2,
        smt/1,
        bool_and/2,
        bool_or/2,
        bool_xor/2,
        bool_channel/4,
        bin_packing/2,
	multi_cumulative/2,
	multi_cumulative/3,
	seq_precede_chain/1,
	seq_precede_chain/2,
	value_precede_chain/2,
	value_precede_chain/3,
	symmetric_all_different/1,
	symmetric_all_distinct/1,
	minimum_arg/2,
	maximum_arg/2,
	all_equal/1,
	all_equal_reif/2,
	#= /2,
	#\= /2,
	#< /2,
	#=< /2,
	#> /2,
	#>= /2,
	#\ /1,
	#/\ /2,
	#\ /2,
	#\/ /2,
	#=> /2,
	#<= /2,
	#<=> /2,
	% geost extras
	% geost_domination_data/3,
	% geost_domination_post/4,
	% programming interface
	fd_batch/1,
	fd_global/3,
	fd_global/4,
	fd_flag/3,
	is_fdset/1,
	empty_fdset/1,
	fdset_parts/4,
	empty_interval/2,
	fdset_interval/3,
	fdset_singleton/2,
	fdset_min/2,
	fdset_max/2,
	fdset_size/2,
	list_to_fdset/2,
	fdset_to_list/2,
	range_to_fdset/2,
	fdset_to_range/2,
	fdset_add_element/3,
	fdset_del_element/3,
	fdset_disjoint/2,
	fdset_intersect/2,
	fdset_intersection/3,
	fdset_intersection/2,
	fdset_member/2,
	fdset_eq/2,
	fdset_subset/2,
	fdset_subtract/3,
	fdset_union/3,
	fdset_union/2,
	fdset_complement/2
	]).

:- use_module(library(atts)).

:- use_module(library(avl), [
        empty_avl/1, 
	avl_fetch/3,
	list_to_avl/2,
	ord_list_to_avl/2,
	avl_to_list/2,
	avl_store/4
	]).

:- use_module(library(lists), [
	append/2,
	is_list/1,
	keyclumped/2,
	last/2,
	max_member/2,
	min_member/2,
	nth0/3,
	nth1/3,
	nth1/4,
	prefix_length/3,
	reverse/2,
	select/3,
	selectchk/3,
	suffix_length/3,
	sumlist/2,
	transpose/2
	]).

:- use_module(library(ordsets), [
	ord_disjoint/2,
	ord_subtract/3,
	ord_intersection/3,
	ord_member/2,
	ord_nonmember/2,
	ord_union/2,
	ord_union/3,
	ord_union/4,
	ord_add_element/3,
	ord_del_element/3
	]).

:- use_module(library(trees), [
	get_label/3,
	list_to_tree/2,
	put_label/4
	]).

:- use_module(library(types), [
	illarg/3,
	illarg/4,
	must_be/4
	]).

:- use_module(library(terms), [
	term_hash/2,
	term_variables_set/2
	]).

:- use_module(library(timeout), [
	time_out/3
	]).

:- use_module(library(ugraphs), [
	vertices_edges_to_ugraph/3,
	reachable/3
	]).


:- op(1200, xfx, --->).
:- op(1200, xfx, [+:,-:,+?,-?]).%% as :-
:- op(900,  xfx, iff).
:- op(760,  yfx, #<=>).
:- op(750,  xfy, #=>).
:- op(750,  yfx, #<=).
:- op(740,  yfx, #\/).
:- op(730,  yfx, #\).
:- op(720,  yfx, #/\).
:- op(710,   fy, #\).
:- op(700,  xfx, [in,in_set]).
:- op(700,  xfx, [#=,#\=,#<,#=<,#>,#>=]).
%- op(600,  xf,  !).  %% [MC] 3.11.1
:- op(550,  xfx, ..). %% higher than +,-,*...
:- op(500,  yfx, \).				% same as \/, /\
:- op(500,   fy, \).				% same as \/, /\
:- op(490,  yfx, ?).				% tighter than \/, /\
:- op(400,  yfx, [/>,/<]).			% divu, divd

:- dynamic
	full_answer/0.

:- volatile
	full_answer/0.

:- public
	'$fd_set_expression'/2,
	'$fd_check_arguments'/2,
	'$fd_install'/5,
	'$fd_global_enqueue'/1,
	'$fd_linear_reif'/3,
	'$fd_square'/3,
	'$fd_product'/3,
	'$fd_quotient'/3,
	'$fd_divide'/3,
	'$fd_modulo'/3,
	'$fd_remainder'/3,
	'$fd_int_times'/3,
	'$fd_cumulative'/3,
	'$fd_ttef_cumulative'/3,
	'$fd_multi_cumulative'/3,
	'$fd_bin_packing'/3,
	'$fd_compact_table'/3,
	'$fd_all_different'/3,
	'$fd_all_distinct'/3,
	'$fd_bc_alldiff'/3,
	'$fd_bc_alldiff_lia'/3,
	'$fd_atleast_le'/3,
	'$fd_pairing'/3,
	'$fd_sorting'/3,
	'$fd_keysorting'/3,
	'$fd_assignment'/3,
	'$fd_assignment_helper'/3,
	'$fd_circuit'/3,
	'$fd_subcircuit'/3,
	'$fd_element'/3,
	'$fd_member'/3,
	'$fd_minmax'/3,
	'$fd_geost'/3,
	'$fd_mddi'/3,
	'$fd_bool_or'/3,
	'$fd_bool_xor'/3,
	'$fd_bool_channel'/3,
	'$fd_ac3intervals'/3,
	'$fd_ac3element'/3,
	'$fd_nvalue'/3,
	'$fd_diffn'/3,
	'$fd_cumulatives'/3,
	'$fd_gcc'/3,
	'$fd_gcc_helper'/3,
	'$fd_lcc'/3,
	'$fd_lex_chain'/3,
	'$fd_gcd_aux'/3,
	'$fd_value_precede_chain'/3,
	'$fd_minimum_arg'/3,
	'$fd_maximum_arg'/3,
	'$fd_all_equal_reif'/3,
        '$fd_trailed_Dfree'/1,
        '$fd_purge'/1.

/***
Some key data structures
========================

fd_attribute(Dvar,DomM,ListsM,AliasesM,AFC)

The variable attribute of this module.

Dvar     : Dvar itself, comes in handy, must be dereferenced
DomM     : Dvar's domain, a dom/4 term wrapped in a mutable.
ListsM   : Dvar's dependencies, a $fdlists/21 term wrapped in a mutable.
AliasesM : Open list of -(offset,attribute) pairs, forming an equality class
AFC      : Accumulated failure count

dom(Set,Min,Max,Size)
---------------------

A variable domain.

Set :  fdset representation
Min :  smallint or 'inf'
Max :  smallint or 'sup'
Size : smallint or 'sup'

$fdlists(Count, Mask, DomIx, DomDaemon, DomGlobal,
	              MinIx, MinDaemon, MinGlobal,
	              MaxIx, MaxDaemon, MaxGlobal,
	              MinMaxIx, MinMaxDaemon, MinMaxGlobal,
	              ValImp0, ValImp1, ValDiseq, ValWatcher, 
	              ValIx, ValDaemon, ValGlobal)
--------------------------------------------------

Variable dependency lists.  XREF ix_item/5, verify_attributes/3, fd_neighbors//[3,4].

global(StateMutable,Constraint,StatusMutable,EntailmentFlag,AttrList,Source)
----------------------------------------------------------------------------

A propagator for a global constraint.

StateMutable : The value is the propagator's current state, i.e. the
               backtrackable, Prolog part of it. The built-in
	       propagators also have non-backtrackable state in C.
Constraint :   The actual constraint.
StatusMutable : The value is an integer, a bitmask of:
                0x1 - linked into dependency lists
                0x2 - in propagation queue
                0x4 - non-idempotent
                0x8 - currently being filtered
                0x60 - reified: 0x20 = tell negative, 0x40 = tell positive, 0x60 = ask
EntailmentFlag : Unbound if not yet entailed, 1 otherwise.
Source : Usually the same as Constraint, but not always.
         For example, some global constraint need auxiliary
	 constraints that should be invisible to the user if he
	 inspects the constraints attached to a domain variable, in
	 which case Source = true. Source is accessed by
	 clpfd:attribute_goal/2, the API invoked by frozen/2 and by
	 the top level when it displays residual constraints.

ix(Ptr,Constraint,StatusMutable,EntailmentFlag,ZeroOne,AttrVector,ZeroOneAttr,Source)
-------------------------------------------------------------------------------------

A propagator for an indexical constraint.

Ptr : a pointer to a struct indexical_info.
ZeroOne : 1 if entailed, 0 if disentailed, unbound otherwise
          Only used for reification, in which case it's a dvar.
AttrVector : Same as constraint, but with arguments replaced by their
             respective attribute terms.
ZeroOneAttr : Attribute term for ZeroOne
Source: See above.

daemon(Global,AttrRef,StatusMutable,EntailmentFlag,QueueIndex,Handle)
----------------------------------------------------------

A daemon for a global constraint.

Global  : global/5 term.
AttrRef : a pointer to the attribute term to which the daemon is attached.
QueueIndex : queue index for the global, if scheduled.
Handle  : a term containing a pointer to the C state.

clause(Watched1,Watched2,StatusMutable,EntailmentFlag,PosLiterals,NegLiterals)
------------------------------------------------------------------------------

A Boolean clause.

Watched     : i>0 (i<0) means watching ith positive (negative) literal
PosLiterals : list of VarAttrPair of positive literals
NegLiterals : list of VarAttrPair pairs of negative literals

disequation(CounterMutable,Coeffs,Variables,RHS)
--------------------------------------------------------------------------------

A disequation Coeffs * Variables #\= RHS

CounterMutable   : number of vars not yet ground
Coeffs    : list of smallint
Variables : list of VarAttrPair pairs
RHS       : smallint

watcher(Watching,VarAttrPair,Tag,EntailmentFlag,Clause)
-------------------------------------------------------------

A watcher for a Boolean clause.

Clause   : clause/6 term.
Tag      : 0 for nary clause, 1,2 for binary clause
AttrRef  : a pointer to the attribute term that we are watching
Watching : i>0 (i<0) means watching ith positive (negative) literal

implication(XVar-XAttr,XTag,YVar-YAttr,YTag)
--------------------------------------------

An implication YLit #=> XLit

XTag     : if 0, then XLit = XVar, else XLit = #\XVar
YTag     : if 1, then YLit = YVar, else YLit = #\YVar
***/

%% extremely crude version: just filter out any non-query vars
project_attributes(_, _) :- full_answer, !.
project_attributes(QueryVars, AttVars) :-
	sort(QueryVars, QueryVars1),
	sort(AttVars, AttVars1),
	ord_subtract(AttVars1, QueryVars1, ElimVars),
	(   foreach(X,ElimVars)
	do  purify_var(X)
	).

attribute_goal(X, Goal) :-
	fd_set(X, Set),
	\+fdset_singleton(Set, _),		% temp created by fdvar=fdvar
	fdset_to_range(Set, Dom1),
	(   full_answer ->
	    all_suspensions(X, Sorted),
	    commafy(Sorted, X in Dom1, Goal)
	;   Goal = (X in Dom1)
	).

all_suspensions(X, Sorted) :-
	var(X),
	get_fd_suspensions_closure(X, ListsOfLists, Equations), !,
	(   foreach(Lists,ListsOfLists),
	    fromto(All,All1,All2,Equations)
	do  Lists =.. [_,_,_|ListLists],
	    fdlists_suspensions(ListLists, All1, All2)
	),	
	sort(All, Sorted).
all_suspensions(_, []).

get_fd_suspensions_closure(X, ListOfSusps, Equations) :-
	get_fd_aliases(X, Aliases),
	(   foreach(Off1-Alias1,Aliases),
	    fromto(ListOfSusps,Susp1,Susp2,[]),
	    param(X,Off0)
	do  Alias1 = v(_,Y,_,SuspM,_,_),
	    (   Y==X
	    ->  Off0 = Off1,
		get_mutable(Susp, SuspM),
		Susp1 = [Susp|Susp2]
	    ;   Susp1 = Susp2
	    )
	),
	(   foreach(Off2-Alias2,Aliases),
	    fromto(Equations,Eq1,Eq2,[]),
	    param(X,Off0)
	do  Alias2 = v(_,Y2,_,_,_,_),
	    (   Y2\==X
	    ->  offset_expr(Off0, X, XOff),
		offset_expr(Off2, Y2, YOff),
	        sort2(XOff, YOff, X1, X2),
		Eq1 = [X1 #= X2|Eq2]
	    ;   Eq1 = Eq2
	    )
	).

offset_expr(0, X, X) :- !.
offset_expr(Off, X, X+Off) :- Off>0, !.
offset_expr(Off, X, X-NOff) :- NOff is -Off.

fdlists_suspensions(ListLists) -->
	(   foreach(List,ListLists)
	do  (   foreach(Item,List)
	    do  suspension(Item)
	    )
	).

suspension(Ix) -->
	{Ix = ix(_,_,_,Ent,_,_,_,Source)},
	{var(Ent), Source\==true}, !,
	propagator_goal(Ix).
suspension(global(_,_,_,Ent,_,Source)) -->
	{var(Ent), Source\==true}, !,
	propagator_goal(global(_,_,_,Ent,_,Source)).
suspension(watcher(_,_,_,Ent,Item)) -->
	{var(Ent)}, !,
	propagator_goal(watcher(_,_,_,Ent,Item)).
suspension(disequation(CountM,Coeffs,Vars,RHS)) -->
	{get_mutable(C,CountM)},
	{C >= 2}, !,
	propagator_goal(disequation(CountM,Coeffs,Vars,RHS)).
suspension(daemon(Item,_,_,Ent,_,_)) -->
	{var(Ent)}, !,
	suspension(Item).
suspension(implication(Y-_,0,X-_,1)) --> {var(Y)}, !,
	[Y #=> X].
suspension(implication(Y-_,1,X-_,1)) --> {var(Y)}, !,
	{sort2(X, Y, X1, Y1)},
	[X1 #\/ Y1].
suspension(implication(Y-_,0,X-_,0)) --> {var(Y)}, !,
	{sort2(X, Y, X1, Y1)},
	[#\X1 #\/ #\Y1].
suspension(implication(Y-_,1,X-_,0)) --> {var(Y)}, !,
	[X #=> Y].
suspension(_) --> [].

sort2(X, Y, X, Y) :- X @=< Y, !.
sort2(X, Y, Y, X).

propagator_goal(watcher(_,_,_,_,Item)) -->
	propagator_goal(Item).
propagator_goal(disequation(_,Coeffs,Vars,RHS)) -->
	(   foreach(V-_,Vars),
	    foreach(V,Vs)
	do  []
	),
	{pretty_scalar_product(scalar_product_reif(Coeffs,Vs,#\=,RHS,1), Goal)},
	[Goal].
propagator_goal(clause(_,_,_,_,PosLiterals,NegLiterals)) -->
	(   foreach(V1-_,PosLiterals),
	    fromto(L1,L2,L3,L4)
	do  {L2 = [V1|L3]}
	),
	(   foreach(V2-_,NegLiterals),
	    fromto(L4,L5,L6,[])
	do  {L5 = [#\(V2)|L6]}
	),
	[bool_or(L1,1)].
propagator_goal(ix(Ptr,_,_,_,ZeroOne,_,_,FDPred)) -->
	{'$fd_indexical_data'(Ptr, Type, Module)},
	{   Module = clpfd,
	    fdpred_exported(FDPred, Constraint) -> true
	;   Constraint = FDPred
	},
	{   Module = clpfd -> ModCon = Constraint
	;   ModCon = Module:Constraint
	},
	propagator_goal_item(Type, ZeroOne, ModCon).
propagator_goal(global(_,_,_,_,_,clpfd:FDPred)) -->
	{fdpred_exported(FDPred, Constraint)}, !,
	[Constraint].
propagator_goal(global(_,_,_,_,_,Source)) -->
	{   Source = clpfd:Ctr -> true
	;   Source = Ctr
	},
	[Ctr].

propagator_goal_item(_, B, C) --> {B==0}, !, [#\ C].
propagator_goal_item(_, B, C) --> {B==1}, !, [C].
propagator_goal_item(_, B, C) --> !, [C #<=> B].

fdpred_exported('ax=t'(A,X,T), A*X #= T).
fdpred_exported('x+y=t'(X,Y,T), X+Y #= T).
fdpred_exported('t+u=c'(T,U,C), T+U #= C).
fdpred_exported('x+c=y'(X,C,Y), X+C #= Y).
fdpred_exported('t=u+c'(T,U,C), T #= U+C).
fdpred_exported('t=<u+c'(T,U,C), T #=< U+C).
fdpred_exported('t\\=u+c'(T,U,C), T #\= U+C).
fdpred_exported('t>=u+c'(T,U,C), T #>= U+C).
fdpred_exported('ax+c=t'(A,X,C,Y), A*X+C #= Y).
fdpred_exported('ax+y=t'(A,X,Y,Z), A*X+Y #= Z).
fdpred_exported('t+u=<c'(T,U,C), T+U #=< C).
fdpred_exported('t+u\\=c'(T,U,C), T+U #\= C).
fdpred_exported('t+u>=c'(T,U,C), T+U #>= C).
fdpred_exported('ax+by=t'(A,X,B,Y,Z), A*X+B*Y #= Z).
fdpred_exported('x+y=u+c'(X,Y,U,C), X+Y #= U+C).
fdpred_exported('x+y+c=z'(X,Y,C,Z), X+Y+C #= Z).
fdpred_exported('ax+y+c=z'(A,X,Y,C,Z), A*X+Y+C #= Z).
fdpred_exported('x+y+z=t'(X,Y,Z,T), X+Y+Z #= T).
fdpred_exported('x+y+z=c'(X,Y,Z,C), X+Y+Z #= C).
fdpred_exported('ax+y+z=t'(A,X,Y,Z,T), A*X+Y+Z #= T).
fdpred_exported('-ax=t'(A,X,T), NA*X #= T) :- NA is -A.
fdpred_exported('oneof(x,y)=z IND'(X,Y,Z), X#=Z #\/ Y#=Z).
fdpred_exported('min(x,y)=z'(X,Y,Z), min(X,Y) #= Z).
fdpred_exported('max(x,y)=z'(X,Y,Z), max(X,Y) #= Z).
fdpred_exported('|x|=y 1'(X,Y), abs(X) #= Y).
fdpred_exported('t=u IND'(X,Y), X #= Y).
fdpred_exported('x\\=y IND'(X,Y), X #\= Y).
fdpred_exported('x=<y IND'(X,Y), X #=< Y).
fdpred_exported('x<y IND'(X,Y), X #< Y).
fdpred_exported('x*x=y'(X,Y), X*X #= Y).
fdpred_exported('x*y=z'(X,Y,Z), X*Y #= Z).
fdpred_exported('x/y=z'(X,Y,Z), X//Y #= Z).
fdpred_exported('x div y=z'(X,Y,Z), X div Y #= Z).
fdpred_exported('x mod y=z'(X,Y,Z), X mod Y #= Z).
fdpred_exported('x rem y=z'(X,Y,Z), X rem Y #= Z).
fdpred_exported(X in_set S, X in R) :-
	fdset_to_range(S, R).
fdpred_exported(in_set_ix(X, Set), X in R) :-
	fdset_to_range(Set, R).
fdpred_exported(domain(X,[[B|E]]), domain(X, B, E)).
fdpred_exported(swap_values(X,Y,I,J), clpfd:swap_values(X,Y,I,J)).

fd_statistics :-
	'$fd_statistics'(0, S0),
	'$fd_statistics'(1, S1),
	'$fd_statistics'(2, S2),
	'$fd_statistics'(3, S3),
	'$fd_statistics'(4, S4),
        format(user_error,
               'Resumptions: ~d\n\
Entailments: ~d\n\
Prunings: ~d\n\
Backtracks: ~d\n\
Constraints created: ~d\n', [S0,S1,S2,S3,S4]).

fd_statistics(Key, Value) :-
	statistics_code(Key, Code),
	'$fd_statistics'(Code, Value).

statistics_code(resumptions, 0).
statistics_code(entailments, 1).
statistics_code(prunings, 2).
statistics_code(backtracks, 3).
statistics_code(constraints, 4).

:- public overflow_action/2.
% [PM] 4.4.0 must be defined before init(fd_initialize) is run by load_foreign_resource(), below. */
overflow_action(Constraint, Code) :- % [MC] SPRM 13682
	Fake is 2*Code-3,
	fd_argument_error(Constraint, 0, Fake).

foreign_resource(clpfd,
        [init(fd_initialize),
	 deinit(fd_deinitialize),
	 prolog_fd_median,
	 prolog_fd_middle,
	 prolog_fd_size,
	 prolog_fd_range,
	 prolog_fd_cons,
	 prolog_fd_dom_complement,
	 prolog_fd_dom_subtract,
	 prolog_fd_dom_intersection,
	 prolog_fd_dom_union,
	 prolog_fd_dom_contains,
	 prolog_fd_dom_insert,
	 prolog_fd_dom_delete,
	 prolog_fd_dom_intersect,
	 prolog_fd_set_expression,
	 prolog_fd_arg_attribute,
	 prolog_fd_dvar_list,
	 prolog_fd_coref,
	 prolog_fd_begin,
	 prolog_fd_check_arguments,
	 prolog_fd_install,
	 prolog_fd_post_reified,
	 prolog_fd_post_global,
	 prolog_fd_post_clause,
	 prolog_fd_post_disequation,
	 prolog_fd_post_equality,
	 prolog_fd_prune_and_enqueue,
	 prolog_fd_find_definition,
	 prolog_fd_indexical_data,
	 prolog_fd_global_enqueue,
	 prolog_fd_statistics,
	 prolog_fd_batch,
	 prolog_fd_hiding,
	 prolog_fd_debug,
	 prolog_fd_overflow,
	 prolog_fd_unify,
	 prolog_fd_in_set,
	 prolog_fd_in_interval,
	 prolog_fd_eval_indexical,
	 prolog_fd_evaluate_indexical,
	 prolog_fd_update_incumbent,
	 prolog_fd_incumbent_bound,
	 prolog_fd_dispatch_global_fast,
	 prolog_fd_trailed_Dfree,
	 prolog_fd_purge,
	 prolog_fd_linear_reif,
	 prolog_fd_square,
	 prolog_fd_product,
	 prolog_fd_quotient,
	 prolog_fd_divide,
	 prolog_fd_modulo,
	 prolog_fd_remainder,
	 prolog_fd_int_times,
	 prolog_fd_cumulative,
	 prolog_fd_ttef_cumulative,
	 prolog_fd_multi_cumulative,
	 prolog_fd_bin_packing,
	 prolog_fd_compact_table,
	 prolog_fd_all_different,
	 prolog_fd_all_distinct,
	 prolog_fd_bc_alldiff,
	 prolog_fd_bc_alldiff_lia,
	 prolog_fd_atleast_le,
	 prolog_fd_pairing,
	 prolog_fd_sorting,
	 prolog_fd_keysorting,
	 prolog_fd_assignment,
	 prolog_fd_assignment_helper,
	 prolog_fd_circuit,
	 prolog_fd_subcircuit,
	 prolog_fd_element,
	 prolog_fd_member,
	 prolog_fd_minmax,
	 prolog_fd_nvalue,
	 prolog_fd_geost,
	 prolog_fd_mddi,
	 prolog_fd_bool_or,
	 prolog_fd_bool_xor,
	 prolog_fd_bool_channel,
	 prolog_fd_ac3intervals,
	 prolog_fd_ac3element,
	 prolog_fd_diffn,
	 prolog_fd_cumulatives,
	 prolog_fd_gcc,
	 prolog_fd_gcc_helper,
	 prolog_fd_lcc,
	 prolog_fd_lex_chain,
	 prolog_fd_gcd_aux,
	 prolog_fd_value_precede_chain,
	 prolog_fd_minimum_arg,
	 prolog_fd_maximum_arg,
	 prolog_fd_all_equal_reif,
	 prolog_fd_delete,
	 prolog_fd_compact_table_common,
	 prolog_fd_ac3intervals_common,
	 prolog_fd_ac3element_common,
	 prolog_fd_mddi_common,
	 prolog_fd_table_compile,
	 prolog_fd_findall_begin,
	 prolog_fd_findall_end,
	 prolog_fd_findall_inserta,
	 prolog_fd_findall_list,
	 prolog_fzn_read_token,
	 prolog_fzn_read_item
	]).

foreign(prolog_fd_median, '$fd_median'(+term,[-integer])). % 4.3
foreign(prolog_fd_middle, '$fd_middle'(+term,[-integer])). % 4.3
foreign(prolog_fd_size, '$fd_size'(+term,-term,[-integer])).
foreign(prolog_fd_range, '$fd_range'(+term,+term,-term,[-integer])).
foreign(prolog_fd_cons, '$fd_cons'(+term,+term,+term,-term,[-integer])).
foreign(prolog_fd_dom_complement, '$fd_dom_complement'(+term,-term)).
foreign(prolog_fd_dom_subtract, '$fd_dom_subtract'(+term,+term,-term)).
foreign(prolog_fd_dom_intersection, '$fd_dom_intersection'(+term,+term,-term)).
foreign(prolog_fd_dom_union, '$fd_dom_union'(+term,+term,-term)).
foreign(prolog_fd_dom_contains, '$fd_dom_contains'(+term,+term)).
foreign(prolog_fd_dom_insert, '$fd_dom_insert'(+term,+term,-term)).
foreign(prolog_fd_dom_delete, '$fd_dom_delete'(+term,+term,-term)).
foreign(prolog_fd_dom_intersect, '$fd_dom_intersect'(+term,+term,[-integer])).
foreign(prolog_fd_set_expression, '$fd_set_expression'(+term,-term)).
foreign(prolog_fd_arg_attribute, '$fd_arg_attribute'(+term,+integer,-term)).
foreign(prolog_fd_dvar_list, '$fd_dvar_list'(+term,+integer)).
foreign(prolog_fd_coref, '$fd_coref'(+term)).
foreign(prolog_fd_begin, '$fd_begin').
foreign(prolog_fd_check_arguments, '$fd_check_arguments'(+term,-term)).
foreign(prolog_fd_install, '$fd_install'(+term,+atom,+integer,+integer,+term)).
foreign(prolog_fd_post_reified, '$fd_post_reified'(+term,+term,+term,+term,+term)).
foreign(prolog_fd_post_global, '$fd_post_global'(+term,+term,+integer,+term,+term,-term)).
foreign(prolog_fd_post_clause, '$fd_post_clause'(+term,+term)).
foreign(prolog_fd_post_disequation, '$fd_post_disequation'(+term,+term,+integer)).
foreign(prolog_fd_post_equality, '$fd_post_equality'(+term,+term,[-integer])).
foreign(prolog_fd_prune_and_enqueue, '$fd_prune_and_enqueue'(+term,+term)).
foreign(prolog_fd_find_definition, '$fd_find_definition'(+term,+atom,-term/*struct def.*/)).
foreign(prolog_fd_indexical_data, '$fd_indexical_data'(+term/*indexical ptr*/,-integer,[-atom])).
foreign(prolog_fd_global_enqueue, '$fd_global_enqueue'(+term)).
foreign(prolog_fd_statistics, '$fd_statistics'(+integer,[-integer])).
foreign(prolog_fd_batch, '$fd_batch'(+term,+term,[-integer])).
foreign(prolog_fd_hiding, '$fd_hiding'(+term,+term,[-integer])).
foreign(prolog_fd_debug, '$fd_debug'(+term,+term,[-integer])).
foreign(prolog_fd_overflow, '$fd_overflow'(+term,+term,[-integer])).
foreign(prolog_fd_unify, '$fd_unify'(+term,+term,[-integer])).
foreign(prolog_fd_in_set, '$fd_in_set'(+term,+term,+integer)).
foreign(prolog_fd_in_interval, '$fd_in_interval'(+term,+term,+term,+integer)).
foreign(prolog_fd_eval_indexical, '$fd_eval_indexical'(+term,-term,+term)).
foreign(prolog_fd_evaluate_indexical, '$fd_evaluate_indexical'([-integer],-term)).
foreign(prolog_fd_update_incumbent, '$fd_update_incumbent'(+term/*struct instance*/,+term,+term)).
foreign(prolog_fd_incumbent_bound, '$fd_incumbent_bound'(+term/*struct instance*/,-term)).
foreign(prolog_fd_dispatch_global_fast,
	'$fd_dispatch_global_fast'(+term,+term,-term,-term,+term,[-integer])).
foreign(prolog_fd_trailed_Dfree, '$fd_trailed_Dfree'(-term)).
foreign(prolog_fd_purge, '$fd_purge'(+term)).

%% dispatch_global_fast/4 targets:

foreign(prolog_fd_linear_reif, '$fd_linear_reif'(+term,-term,-term)).
foreign(prolog_fd_square, '$fd_square'(+term,-term,-term)).
foreign(prolog_fd_product, '$fd_product'(+term,-term,-term)).
foreign(prolog_fd_quotient, '$fd_quotient'(+term,-term,-term)).
foreign(prolog_fd_divide, '$fd_divide'(+term,-term,-term)).
foreign(prolog_fd_modulo, '$fd_modulo'(+term,-term,-term)).
foreign(prolog_fd_remainder, '$fd_remainder'(+term,-term,-term)).
foreign(prolog_fd_int_times, '$fd_int_times'(+term,-term,-term)).
foreign(prolog_fd_cumulative, '$fd_cumulative'(+term,-term,-term)).
foreign(prolog_fd_ttef_cumulative, '$fd_ttef_cumulative'(+term,-term,-term)).
foreign(prolog_fd_multi_cumulative, '$fd_multi_cumulative'(+term,-term,-term)).
foreign(prolog_fd_bin_packing, '$fd_bin_packing'(+term,-term,-term)).
foreign(prolog_fd_compact_table, '$fd_compact_table'(+term,-term,-term)).
foreign(prolog_fd_all_different, '$fd_all_different'(+term,-term,-term)).
foreign(prolog_fd_all_distinct, '$fd_all_distinct'(+term,-term,-term)).
foreign(prolog_fd_bc_alldiff, '$fd_bc_alldiff'(+term,-term,-term)).
foreign(prolog_fd_bc_alldiff_lia, '$fd_bc_alldiff_lia'(+term,-term,-term)).
foreign(prolog_fd_atleast_le, '$fd_atleast_le'(+term,-term,-term)).
foreign(prolog_fd_pairing, '$fd_pairing'(+term,-term,-term)).
foreign(prolog_fd_sorting, '$fd_sorting'(+term,-term,-term)).
foreign(prolog_fd_keysorting, '$fd_keysorting'(+term,-term,-term)).
foreign(prolog_fd_assignment, '$fd_assignment'(+term,-term,-term)).
foreign(prolog_fd_assignment_helper, '$fd_assignment_helper'(+term,-term,-term)).
foreign(prolog_fd_circuit, '$fd_circuit'(+term,-term,-term)).
foreign(prolog_fd_subcircuit, '$fd_subcircuit'(+term,-term,-term)).
foreign(prolog_fd_element, '$fd_element'(+term,-term,-term)).
foreign(prolog_fd_member, '$fd_member'(+term,-term,-term)).
foreign(prolog_fd_minmax, '$fd_minmax'(+term,-term,-term)).
foreign(prolog_fd_geost, '$fd_geost'(+term,-term,-term)).
foreign(prolog_fd_mddi, '$fd_mddi'(+term,-term,-term)).
foreign(prolog_fd_bool_or, '$fd_bool_or'(+term,-term,-term)).
foreign(prolog_fd_bool_xor, '$fd_bool_xor'(+term,-term,-term)).
foreign(prolog_fd_bool_channel, '$fd_bool_channel'(+term,-term,-term)).
foreign(prolog_fd_ac3intervals, '$fd_ac3intervals'(+term,-term,-term)).
foreign(prolog_fd_ac3element, '$fd_ac3element'(+term,-term,-term)).
foreign(prolog_fd_nvalue, '$fd_nvalue'(+term,-term,-term)).
foreign(prolog_fd_diffn, '$fd_diffn'(+term,-term,-term)).
foreign(prolog_fd_cumulatives, '$fd_cumulatives'(+term,-term,-term)).
foreign(prolog_fd_gcc, '$fd_gcc'(+term,-term,-term)).
foreign(prolog_fd_gcc_helper, '$fd_gcc_helper'(+term,-term,-term)).
foreign(prolog_fd_lcc, '$fd_lcc'(+term,-term,-term)).
foreign(prolog_fd_lex_chain, '$fd_lex_chain'(+term,-term,-term)).
foreign(prolog_fd_gcd_aux, '$fd_gcd_aux'(+term,-term,-term)).
foreign(prolog_fd_value_precede_chain, '$fd_value_precede_chain'(+term,-term,-term)).
foreign(prolog_fd_minimum_arg, '$fd_minimum_arg'(+term,-term,-term)).
foreign(prolog_fd_maximum_arg, '$fd_maximum_arg'(+term,-term,-term)).
foreign(prolog_fd_all_equal_reif, '$fd_all_equal_reif'(+term,-term,-term)).
foreign(prolog_fd_delete, '$fd_delete'(+term,-term,+atom)).
foreign(prolog_fd_compact_table_common, '$fd_compact_table_common'(+term,+term,+integer,-term)).
foreign(prolog_fd_ac3intervals_common, '$fd_ac3intervals_common'(+term,+term,+term,+integer,-term)).
foreign(prolog_fd_ac3element_common, '$fd_ac3element_common'(+term,+term,+integer,-term)).
foreign(prolog_fd_mddi_common, '$fd_mddi_common'(+integer,+term,+term,+term,+integer,-term)).
foreign(prolog_fd_table_compile, '$fd_table_compile'(+term,-term,-term,-term,+term,-term,-term,-term,-term,+string,+string,-term,[-integer])).
foreign(prolog_fd_findall_begin, '$fd_findall_begin'(-term/* struct fd_findall ptr */, +term)).
foreign(prolog_fd_findall_end, '$fd_findall_end'(+term/* struct fd_findall ptr */)).
foreign(prolog_fd_findall_inserta, '$fd_findall_inserta'(+term/* struct fd_findall ptr */, +term, [-integer])).
foreign(prolog_fd_findall_list, '$fd_findall_list'(+term/* struct fd_findall ptr */, -term)).
foreign(prolog_fzn_read_token, fzn_read_token(+address, +integer, [-integer], -term, -integer)).
foreign(prolog_fzn_read_item,   fzn_read_item(+address, -term, [-integer])).

:- load_foreign_resource(library(system(clpfd))).

:- ensure_loaded('clpfd/fdsets').
:- ensure_loaded('clpfd/ixq').
:- ensure_loaded('clpfd/enum').
:- ensure_loaded('clpfd/compiler').
:- ensure_loaded('clpfd/lib').
:- ensure_loaded('clpfd/automaton').
:- ensure_loaded('clpfd/regular').
:- ensure_loaded('clpfd/geost').
:- ensure_loaded('clpfd/mddi').

