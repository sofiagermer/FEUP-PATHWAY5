/* Copyright(C) 1999, Swedish Institute of Computer Science */

/****************************************************************/
/* case/[3,4]            					*/
/****************************************************************/

% Consistency rules:
% VARS(DAG) = VARS(Template)
% VARS(Tuples) \disjoint VARS(Template)
% every ID integer and unique
% all paths complete

case(Template, Tuples, Dag) :-
	case(Template, Tuples, Dag, [], case(Template,Tuples,Dag)).

case(Template, Tuples, Dag, Options) :-
	case(Template, Tuples, Dag, Options, case(Template,Tuples,Dag,Options)).

% Also called by smt/1
% A "source" is in fact an interval B..E
case([], _, [], _, _) :- !.	% nullary case :-)
case(Template1, Tuples1, Dag1, Options, Posted1) :-
	(   foreach(node(ID,Var,Edges1),Dag1),
	    foreach(node(ID,Var,Edges2),Dag2)
	do  (   foreach(Edge1,Edges1),
		foreach(edge(Source,Linles,Target),Edges2)
	    do  (   Edge1 = Source-Linles-Target
		->  true
		;   Edge1 = Source-Target,
		    simple(Target)
		->  Linles = []
		;   Edge1 = Source-Linles
		->  Target = []
		;   Edge1 = Source, Source = (_.._)
		->  Linles = [],
		    Target = []
		;   Edge1 = Linles
		->  Source = [],
		    Target = []
		)
	    )
	),
	(   foreach(O,Options),
	    fromto(SCPs,SCPs1,SCPs2,[])
	do  (   O = scalar_product(_,_,_,_)
	    ->  SCPs1 = [O|SCPs2]
	    ;   SCPs1 = SCPs2
	    )
	),
	mddi_delinearize(Template1, Tuples1, Dag2, SCPs, Template3, Tuples3, Dag3),
	mddi_build_case(Posted1, Template3, Tuples3, Dag3, Posted2),
	mddi(Template3, Tuples3, Dag3, Posted2).

mddi_build_case(Posted1, Template, Tuples, Dag1, case(Template,Tuples,Dag2)) :-
	functor(Posted1, case, _), !,
	(   foreach(node(ID,Var,Edges1),Dag1),
	    foreach(node(ID,Var,Edges2),Dag2)
	do  (   foreach(edge(Filter,[],To),Edges1),
		foreach(Filter-To,Edges2)
	    do  true
	    )
	).	
mddi_build_case(Posted, _, _, _, Posted).

/* The idea:
   For every level i with var v:
   - if there is no linle on any edge from i to i+1, fine.
   - otherwise:
   * introduce level i' between i and i+1, in template and tuples
   * i' corresponds to a variable v' that identifies relevant edges
   * for every edge from level i with no linle (n1 to n2 if v in a..b)
     replace it by (n1 to n1' if v in a..b) + (n1' to n2 if v' in 0..0)
   * for every edge from level i with linle SCP (n1 to n2 if v in a..b)
     replace it by (n1 to n1' if v in a..b) + (n1' to n2 if v' in j..j)
     where j is unique for this level and > 0
     and post an external constraint (v' #= j #=> SCP)
*/
mddi_delinearize(Template1, Tuples1, Dag1, SCPs1, Template2, Tuples2, Dag2) :-
	term_variables(Template1, Template1V),
	(   foreach(Tuple,Tuples1),
	    foreach(TupleV,Tuples1V),
	    param(Template1, Template1V)
	do  copy_term(Template1-Template1V, Tuple-TupleV)
	),
	transpose(Tuples1V, Tuples1T),
	(   foreach(Node,Dag1),
	    foreach(Key-Node,KN1),
	    param(Template1V)
	do  Node = node(_,Var,_),
	    var_nth(Var, Template1V, 1, Key)
	),
	keysort(KN1, KN2),
	keyclumped(KN2, KN3),
	mddi_delinearize(Template1V, Tuples1T, KN3, 1, Template2, Tuples2T, Dag2, SCPs2),
	append(SCPs1, SCPs2, SCPs3),
	transpose(Tuples2T, Tuples2),
	post_scp(Template2, Tuples2, SCPs3),
	(   SCPs2 = [] -> true
	;   mddi_ground_nodes(Dag2)
	).

mddi_ground_nodes(Dag) :-
	(   foreach(node(ID,_,_),Dag),
	    fromto(Is1,Is2,Is3,[]),
	    fromto(Xs1,Xs2,Xs3,[])
	do  (   integer(ID) -> Is2 = [ID|Is3], Xs2 = Xs3
	    ;   Is2 = Is3, Xs2 = [ID|Xs3]
	    )
	),
	max_member(Max, Is1),
	I is Max+1,
	(   foreach(X,Xs1),
	    count(X,I,_)
	do  true
	).

post_scp(Template, Tuples, SCPs) :-
	(   foreach(Tuple,Tuples),
	    param(Template,SCPs)
	do  (   foreach(SCP,SCPs),
		param(Template,Tuple)
	    do  copy_term(Template-SCP, Tuple-Ctr),
		call(Ctr)
	    )
	).

mddi_delinearize([], [], [], _, [], [], [], []).
mddi_delinearize([Te|Template1], [Tu|Tuples1T], [I-Nodes|KN], J, [Te|Template2], [Tu|Tuples2T], Dag2, SCPs) :-
	I > J, !,
	K is J+1,
	mddi_delinearize(Template1, Tuples1T, [I-Nodes|KN], K, Template2, Tuples2T, Dag2, SCPs).
mddi_delinearize([Te|Template1], [Tu|Tuples1T], [J-Nodes|KN], J, [Te|Template2], [Tu|Tuples2T], Dag1, SCPs) :-
	mdd_free_of_linle(Nodes), !,
	append(Nodes, Dag2, Dag1),
	K is J+1,
	mddi_delinearize(Template1, Tuples1T, KN, K, Template2, Tuples2T, Dag2, SCPs).
mddi_delinearize([Te|Template1], [Tu|Tuples1T], [J-Nodes|KN], J, [Te,Te1|Template2], [Tu,Tu1|Tuples2T], Dag1, SCPs1) :-
	(   foreach(node(ID,Var,Edges),Nodes),
	    foreach(node(ID,Var,Edges1),Nodes1),
	    fromto(Nodes2,Nodes3,Nodes6,[]),
	    fromto(SCPs1,SCPs2,SCPs7,SCPs8),
	    fromto(1,Cnt1,Cnt4,_),
	    param(Te1)
	do  (   foreach(edge(Filter,Linles,Target),Edges),
		foreach(edge(Filter,[],Mid),Edges1),
		fromto(Nodes3,Nodes4,Nodes5,Nodes6),
		fromto(SCPs2,SCPs3,SCPs6,SCPs7),
		fromto(Cnt1,Cnt2,Cnt3,Cnt4),
		param(Te1)
	    do  Nodes4 = [node(Mid,Te1,[edge(Select,[],Target)])|Nodes5],
		(   Linles = []
		->  Select = (0..0),
		    Cnt2 = Cnt3,
		    SCPs3 = SCPs6
		;   Select = (Cnt2..Cnt2),
		    Cnt3 is Cnt2+1,
		    (   foreach(Linle,Linles),
			fromto(SCPs3,SCPs4,SCPs5,SCPs6),
			param(Te1,Cnt2)
		    do  SCPs4 = [(Te1 #= Cnt2 #=> Linle)|SCPs5]
		    )
		)
	    )
	),
	append(Nodes1, Nodes2, Nodes12),
	append(Nodes12, Dag2, Dag1),
	length(Tu, N),
	length(Tu1, N),
	K is J+1,
	mddi_delinearize(Template1, Tuples1T, KN, K, Template2, Tuples2T, Dag2, SCPs8).

mdd_free_of_linle(Nodes) :-
	(   foreach(node(_,_,Edges),Nodes)
	do  (   foreach(edge(_,[],_),Edges)
	    do  true
	    )
	).

mddi(Template, Tuples, Dag, Posted) :-
	Goal = mddi(Template,Tuples,Dag,Posted),
	term_variables(Template, Vars), % term_variables_set would NOT work
	sort(Vars, TemplateVars),
	term_variables_set(Dag, DagVars),
	term_variables_set(Tuples, TuplesVars),
	(   DagVars==TemplateVars -> true
	;   illarg(consistency(Template,Dag,''), Posted, 3)
	),
	(   ord_disjoint(TuplesVars, TemplateVars) -> true
	;   illarg(consistency(Template,Tuples,''), Posted, 2)
	),
	(   mddi_compile(Dag, Vars, Nodes, Edges, VarVals, Sets, Posted) -> true
	;   illarg(consistency(Template,Dag,inconsistent_paths), Posted, 3)
	),
	mddi_post(Tuples, Template, Vars, Nodes, Edges, VarVals, Sets, Goal, Posted).
	
mddi_post(Tuples1, Template, Vars, Nodes, Edges, VarVals, Sets, Goal, Posted) :-
	length(Vars, NVars),
	length(Tuples1, NT),
	'$fd_mddi_common'(NVars, Nodes, Edges, VarVals, NT, Common), % Common = state([_ | '$free'(Ptr)], 0)
	(   foreach(Row1,Tuples1),
	    param(Template,Vars,Sets,Goal,Common,Posted)
	do  copy_term(Template-Vars, Row1-Row2),
	    (   foreach(X,Row2),
		foreach(X-XA,Row3),
		foreach(none(X),Susp),
		foreach(Set,Sets),
		param(Goal)
	    do  arg_attribute(X, XA, Goal, 0),
		prune_and_propagate(X, Set)
	    ),
	    % fd_global(Goal, state(Row3,Common, 0/*trail_top*/,_Handle,0), Susp)
	    fd_global_internal(Goal,
	    		       state(Row3,Common, 0/*trail_top*/,_Handle,0),
	    		       Susp, _, clpfd:Posted, 0)
	).

mddi_compile(Dag, Vars, Nodes, Edges, VarVals, Sets, Goal) :-
	empty_avl(ID2Index0),
	mddi_map_nodes(Dag, Nodes, Pivots0, 0, ID2Index0, ID2Index, Vars, Goal),
	sort(Pivots0, Pivots1),
	keyclumped(Pivots1, Pivots2),
	compensate_for_inf(Pivots2, Pivots3),
	ord_list_to_avl(Pivots3, Node2P),
	mddi_map_rest(Dag, Nodes, Edges1, VarVals1, 0, _, ID2Index, Node2P, Goal),
	keysort(VarVals1, VarVals2),
	keyclumped(VarVals2, VarVals3),
	(   foreach(varval(Var,_,B,E)-Cl,VarVals3),
	    foreach(varval(Var,B,E),VarVals),
	    foreach(Var-(B..E),KL1),
	    count(Ix,0,_)
	do  (   foreach(Ix,Cl),
		param(Ix)
	    do  true
	    )
	),
	keysort(Edges1, Edges2), % sort by inc. varval
	(   foreach(_-Edge2,Edges2),
	    foreach(Edge,Edges)
	do  Edge2 = edge(Source,[],Dest,U),
	    Edge = edge(Source,Dest,U)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(_-Vals,KL3),
	    foreach(Set,Sets)
	do  (   foreach(R1,Vals),
		foreach(S1,SetParts)
	    do  range_to_fdset(R1, S1)
	    ),
	    fdset_union(SetParts, Set)
	).

compensate_for_inf(KL1, KL2) :-
	(   foreach(N-Cl1,KL1),
	    foreach(N-Cl3,KL2)
	do  (   select(inf, Cl1, Cl2) -> Cl3 = slow([inf|Cl2])
	    ;   member(sup, Cl1) -> Cl3 = slow(Cl1)
	    ;   Cl3 = fast(Cl1)
	    )
	).

mddi_map_nodes([], [], [], _, A, A, _, _).
mddi_map_nodes([node(ID,Var,SymEdges)|Nodes], [N|Ns], Pivots0, I, A0, A, Vars, Goal) :-
	must_be(ID, integer, Goal, 0),
	var_nth(Var, Vars, 0, N),
	avl_store(ID, A0, I-N, A1),
	J is I+1,
	(   foreach(edge(B..E,_,_),SymEdges),
	    fromto(Pivots0,[N-B,N-E1|Pivots1],Pivots1,Pivots),
	    param(N)
	do  fdinc(E, E1, 1)
	),
	mddi_map_nodes(Nodes, Ns, Pivots, J, A1, A, Vars, Goal).

mddi_map_rest([], [], [], [], Bot, Bot, _, _, _).
mddi_map_rest([node(_,_,SymEdges)|Nodes], [N|Ns], Edges0, VarVals0, Source, Bot, ID2Index, Node2P, Goal) :-
	(   foreach(SymEdge,SymEdges),
	    fromto(VarVals0,VarVals1,VarVals3,VarVals4),
	    fromto(Edges0,Edges1,Edges3,Edges4),
	    param(N,Source,Bot,ID2Index,Node2P)
	do  (   SymEdge = edge(Key,[],[]) -> Dest = Bot
	    ;   SymEdge = edge(Key,[],Y) ->  avl_fetch(Y, ID2Index, Dest-_)
	    ),
	    avl_fetch(N, Node2P, Pivots),
	    mddi_fragment(Pivots, Key, Vals),
	    (   foreach(B..E,Vals),
		fromto(VarVals1,[varval(N,Aux,B,E)-U|VarVals2],VarVals2,VarVals3),
		fromto(Edges1,[U-edge(Source,[],Dest,U)|Edges2],Edges2,Edges3),
		param(N,Source,Dest)
	    do  (integer(B) -> Aux = B ; Aux = 0.0)
	    )
	),
	Source1 is Source+1,
	mddi_map_rest(Nodes, Ns, Edges4, VarVals4, Source1, Bot, ID2Index, Node2P, Goal).

mddi_fragment(_, Min..Min, [Min..Min]) :- !.
mddi_fragment(fast(Pivots), Min..Max, Vals) :-
	mddi_fragment_fast(Pivots, Min, Max, Vals, []).
mddi_fragment(slow(Pivots), Min..Max, Vals) :-
	mddi_fragment(Pivots, Min, Max, Vals, []).

mddi_fragment([P|Ps], Min, Max) --> {\+le(Min,P)}, !,
	mddi_fragment(Ps, Min, Max).
mddi_fragment([P,Q|Ps], Min, Max) --> {le(P,Max)}, !, [P..R],
	{fdinc(Q, R, -1)},
	mddi_fragment([Q|Ps], Min, Max).
mddi_fragment(_, _, _) --> [].

mddi_fragment_fast([P|Ps], Min, Max) --> {Min>P}, !,
	mddi_fragment_fast(Ps, Min, Max).
mddi_fragment_fast([P,Q|Ps], Min, Max) --> {P=<Max}, !, [P..R],
	{R is Q-1},
	mddi_fragment_fast([Q|Ps], Min, Max).
mddi_fragment_fast(_, _, _) --> [].

fdinc(inf, inf, _) :- !.
fdinc(sup, sup, _) :- !.
fdinc(X, Y, C) :-
	Y is X+C.

/****************************************************************/
/* table/[2,3]           					*/
/****************************************************************/

table(Tuples, Extension1) :-
	table(Tuples, Extension1, [], table(Tuples, Extension1, [])).

table(Tuples, Extension1, Options) :-
	table(Tuples, Extension1, Options, table(Tuples, Extension1, Options)).

% also called by automaton/9
table(Tuples, Extension1, Options, Goal) :-
	must_be(Tuples, proper_list(proper_list), Goal, 1),
	must_be(Extension1, proper_list(proper_list), Goal, 2),
	must_be(Options, proper_list(callable), Goal, 3),
	(   foreach(Opt,Options),
	    fromto(opt(leftmost,default),Opt0,Opt1,opt(Order,Method)),
	    param(Goal)
	do  (   table_option(Opt, Opt0, Opt1) -> true
	    ;   illarg(domain(term,table_option), Goal, 3, Opt)
	    )
	),
	append(Tuples, Extension1, Rows),
	(   (   foreach(Row,Rows),
		param(Arity)
	    do  length(Row, Arity)
	    )
	->  table_checked(Arity, Tuples, Extension1, Order, Method, Goal)
	;   illarg(consistency(Tuples,Extension1,'not same arity'), Goal, 0)
	).

table_checked(_, [], _, _, _, _) :- !.
table_checked(_, _, [], _, _, _) :- !, fail.
table_checked(0, _, _, _, _, _) :- !.
table_checked(_, Tuples, Extension1, Order, Method, Goal) :-
	transpose(Tuples, TuplesT),
	(   foreach(TupleT,TuplesT),
	    foreach(Domain1,Domains1)
	do  (   foreach(Var,TupleT),
		foreach(Set,Sets)
	    do  fd_set(Var, Set)
	    ),
	    fdset_union(Sets, Domain1)
	),
	'$fd_table_compile'(Extension1, Extension2,
			    LitExtension, Literals,
			    Domains1, Domains2,
			    EltValues,
			    Template1, Template2,
			    Order, Method,
			    Culprit, Error),
	(   Error = 0 -> true
	;   set_expression_check(Culprit, _, Goal, 2)
	),
	LitExtension-EltValues \== []-[],
	(   foreach(Tuple1,Tuples),
	    foreach(Tuple2,Tuplesb),
	    param(Template1,Template2)
	do  copy_term(Template1-Template2, Tuple1-Tuple2)
	),
	transpose(Tuplesb, TuplesbT),
	(   foreach(TuplebT,TuplesbT),
	    foreach(Domain2,Domains2)
	do  domain(TuplebT, Domain2)
	),
	table_trim_extension(Goal, Extension2, Goal2),
	length(Template2, Arity),
	(   EltValues\==[]
	->  table_element(Tuplesb, EltValues, Goal2)
	;   LitExtension = [_] -> true
	;   Arity < 2 -> true
	;   Method\==default
	->  table_case(Tuplesb, LitExtension, Literals, Goal2)
	;   Arity=:=2
	->  table_binary(Tuplesb, LitExtension, Literals, Goal2)
	;   table_compact(Tuplesb, LitExtension, Literals, Goal2)
	).

table_trim_extension(table(Tuples,_), Extension, table(Tuples,Extension)) :- !.
table_trim_extension(table(Tuples,_,Options), Extension, table(Tuples,Extension,Options)) :- !.
table_trim_extension(table(Tuples,_,Options,Goal1), Extension, table(Tuples,Extension,Options,Goal2)) :- !,
	table_trim_extension(Goal1, Extension, Goal2).
table_trim_extension(Goal, _, Goal).

table_option(order(Order), opt(_,Method), opt(Order,Method)) :-
	memberchk(Order, [leftmost,id3]).
table_option(method(Method), opt(Order,_), opt(Order,Method)) :-
	memberchk(Method, [default,noaux,aux]).

table_case(Tuples, LitExtension, Literals, Goal) :-
	length(LitExtension, N),
	empty_avl(M2I0),
	mdd_from_rows(N, LitExtension, [], Id, (M2I0,M2I0), Map),
	mdds_to_nodes_edges([Id], Vs, Nodes, Edges0, Map),
	(   foreach(V,Vs),
	    count(V,0,_)
	do  true
	),
	(   foreach(edge(From,To,Lit),Edges0),
	    foreach(Lit-edge(From,To,Lit),Edges1),
	    foreach(_-Edge3,Edges2),
	    foreach(Edge3,Edges3)
	do  true		% Edges must be sorted by (1) Lit, (2) From
	),
	keysort(Edges1, Edges2),
	last(Nodes, Arity1),
	Arity is Arity1+1,
	length(Tuples, NT),
	'$fd_mddi_common'(Arity, Nodes, Edges3, Literals, NT, Common), % Common = state([_ | '$free'(Ptr)], 0)
	(   foreach(Tuple,Tuples),
	    param(Goal,Common)
	do  (   foreach(X,Tuple),
		foreach(X-XA,TupleA),
		foreach(none(X),Susp),
		param(Goal)
	    do  arg_attribute(X, XA, Goal, 0)
	    ),
	    fd_global_internal(mddi(_,_,_,_),
	    		       state(TupleA,Common, 0/*trail_top*/,_Handle,0),
	    		       Susp, _, clpfd:Goal, 0)
	).

mdd_from_rows(1, [Row|Tail], Tail, Id, Map0, Map) :- !,
	mdd_from_row(Row, 0, Id, Map0, Map).
mdd_from_rows(N, Rows, Tail, Id, Map0, Map) :-
	M is N // 2,
	O is N-M,
	mdd_from_rows(M, Rows, Mid, Id1, Map0, Map1),
	mdd_from_rows(O, Mid, Tail, Id2, Map1, Map2),
	mdd_join(Id1, Id2, Id, Map2, Map).

mdd_from_row([], D, Id, Map0, Map) :-
	mdd_to_id(D, [], Id, Map0, Map).
mdd_from_row([Lits|Row], D, Id, Map0, Map) :-
	D1 is D+1,
	mdd_from_row(Row, D1, Id1, Map0, Map1),
	(   foreach(L,Lits),
	    foreach(L-Id1,Children),
	    param(Id1)
	do  true
	),
	mdd_to_id(D, Children, Id, Map1, Map).

mdd_join(Id1, Id2, Id1, Map , Map) :-
	Id1 == Id2, !.
mdd_join(Id1, Id2, Id, Map0, Map) :-
	id_to_mdd(Id1, D, Ch1, Map0),
	id_to_mdd(Id2, D, Ch2, Map0),
	append(Ch1, Ch2, Ch3),
	keysort(Ch3, Ch4),
	join_children(Ch4, Ch5, Map0, Map1),
	mdd_to_id(D, Ch5, Id, Map1, Map).

join_children([], [], Map, Map).
join_children([L-X,L-Y|Ch1], [L-XY|Ch2], Map0, Map) :- !,
	mdd_join(X, Y, XY, Map0, Map1),
	join_children(Ch1, Ch2, Map1, Map).
join_children([L-X|Ch1], [L-X|Ch2], Map0, Map) :-
	join_children(Ch1, Ch2, Map0, Map).

mdds_to_nodes_edges([], [], [], [], _).
mdds_to_nodes_edges([Id], [V], [], [], Map) :-
	Id = id(V),
	id_to_mdd(Id, _, [], Map), !.
mdds_to_nodes_edges([Id|Ids0], [V|Vs], [D|Ds], Edges0, Map) :-
	Id = id(V),
	id_to_mdd(Id, D, Ch, Map),
	(   foreach(L-X,Ch),
	    foreach(X,Xs),
	    fromto(Edges0,Edges1,Edges2,Edges3),
	    param(Id)
	do  mdd_edge(Id, X, L, Edges1, Edges2)
	),
	sort(Xs, XSet),
	sort(Ids0, IdSet),
	ord_subtract(XSet, IdSet, Diff),
	append(Ids0, Diff, Ids),
	mdds_to_nodes_edges(Ids, Vs, Ds, Edges3, Map).

mdd_edge(id(A), id(B), L) --> [edge(A,B,L)].

mdd_to_id(D, Children, id(Id), (M2I,I2M), (M2I,I2M)) :-
	avl_fetch(mdd(D,Children), M2I, id(Id)), !.
mdd_to_id(D, Children, id(Id), (M2I0,I2M0), (M2I,I2M)) :-
	avl_store(mdd(D,Children), M2I0, id(Id), M2I),
	avl_store(id(Id), I2M0, mdd(D,Children), I2M).

id_to_mdd(id(Id), D, Children, (_,I2M)) :-
	avl_fetch(id(Id), I2M, mdd(D,Children)).

table_element(VarTuples, Values, Goal) :-
	list_to_fdset(Values, Set),
	(   foreach([X,Y],VarTuples),
	    param(Goal,Values,Set)
	do  dc_int_element(X, Values, Set, Y, Goal)
	).

table_binary(VarTuples, LitExtension, Literals, Goal) :-
	flatten_extension(LitExtension, ExtensionXY0, ExtensionYX0),
	sort(ExtensionXY0, ExtensionXY),
	sort(ExtensionYX0, ExtensionYX),
	length(VarTuples, NT),
	'$fd_ac3intervals_common'(ExtensionXY, ExtensionYX, Literals, NT, PSet), % PSet = state([_ | '$free'(Ptr)], 0)
	Template = ac3intervals(VarTuples,extension(ExtensionXY,Literals)),
	(   foreach([X,Y],VarTuples),
	    param(Goal,Template,PSet)
	do  arg_attribute(X, XA, Goal, 1),
	    arg_attribute(Y, YA, Goal, 1),
	    fd_global_internal(Template,
			       state([X-XA,Y-YA],PSet,_Handle,0),
			       [none(X),none(Y)], _, clpfd:Goal, 0)
	).

flatten_extension(LitExtension, ExtensionXY0, ExtensionYX0) :-
	(   foreach([Xs,Ys],LitExtension),
	    fromto(ExtensionXY0,ExtensionXY1,ExtensionXY9,[]),
	    fromto(ExtensionYX0,ExtensionYX1,ExtensionYX9,[])
	do  (   foreach(X,Xs),
		fromto(ExtensionXY1,ExtensionXY2,ExtensionXY8,ExtensionXY9),
		fromto(ExtensionYX1,ExtensionYX2,ExtensionYX8,ExtensionYX9),
		param(Ys)
	    do  (   foreach(Y,Ys),
		    fromto(ExtensionXY2,ExtensionXY3,ExtensionXY7,ExtensionXY8),
		    fromto(ExtensionYX2,ExtensionYX3,ExtensionYX7,ExtensionYX8),
		    param(X)
		do  ExtensionXY3 = [[X,Y]|ExtensionXY7],
		    ExtensionYX3 = [[Y,X]|ExtensionYX7]
		)
	    )
	).		

table_compact(VarTuples, LitExtension, Literals, Goal) :-
	length(VarTuples, NT),
	'$fd_compact_table_common'(LitExtension, Literals, NT, TSet), % TSet = state([_ | '$free'(Ptr)], 0)
	Constraint = compact_table(VarTuples,extension(LitExtension,Literals)),
	(   foreach(VarTuple,VarTuples),
	    param(Goal,TSet,Constraint)
	do  (   foreach(V,VarTuple),
		foreach(V-VA,VATuple),
		foreach(none(V),Susp),
		param(Goal)
	    do  arg_attribute(V, VA, Goal, 1)
	    ),
	    fd_global_internal(Constraint,
			       state(VATuple,TSet,0/*trail*/,_Handle,0),
			       Susp, _, clpfd:Goal, 0)
	).
