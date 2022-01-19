/* Copyright(C) 2020, RISE */

/***
%% Kernel ::=    kernel(SourcesSinks,Arcs)

%% Regex ::=    I // integer
%%         |    *(Regex) // Kleene star: zero or more
%%         |    +(Regex) // Kleene plus: one or more
%%         |    ?(Regex) // zero or one
%%         |    (Regex /\ Regex) // intersection
%%         |    (Regex \/ Regex) // union
%%         |    (Regex \  Regex) // difference
%%         |    (Regex + Regex) // concatenation
%%         |    [Regex,Regex,...] // concatenation of list of regex
%%         |    {Regex,Regex,...} // union over set of regex
%%        N.B.  \(Regex) // complement NOT provided because it should be wrt. an alphabet

%. regex_kernel(+Regex, -Kernel)
%%
%% Computes the normalized kernel that recognizes Regex.

%. kernel_closure(+Kernel, -Closure)
%%
%% Computes the Kleene closure of a kernel.

%. kernel_intersection(+Kernel1, +Kernel2, -Intersection)
%%
%% Computes the intersection of two kernels.

%. kernel_union(+Kernel1, +Kernel2, -Union)
%%
%% Computes the union of two kernels.

%. kernel_difference(+Kernel1, +Kernel2, -Difference)
%%
%% Computes the difference of two kernels.

%. kernel_concatenation(+Kernel1, +Kernel2, -Concatenation)
%%
%% Computes the concatenation of two kernels.

%. kernel_normalize(+Kernel1, -Kernel2)
%%
%% Makes a kernel determinate if need be, and minimize it.

%. kernel_string(+Kernel, -String)
%%
%% Generate a string that the kernel recognizes. Enumerate all strings on backtracking.
%% N.B. Kernel must be normalized.
***/

regular(Xs, Regexp) :-
	Goal = regular(Xs, Regexp),
	must_be(Xs, proper_list, Goal, 1),
	regex_kernel(Regexp, Kernel),
	Kernel = kernel(SourcesSinks, Arcs),
	automaton_common(Xs, _, Xs, SourcesSinks, Arcs, [], [], [], [], Goal).

regex_kernel(Regexp, Kernel) :-
	ground(Regexp),
	empty_avl(AVL),
	regex_kernel(Regexp, Kernel, AVL, _).

regex_kernel(Regexp, Kernel, AVL0, AVL) :-
	avl_fetch(Regexp, AVL0, Kernel), !,
	AVL = AVL0.
regex_kernel(Regexp, Kernel, AVL0, AVL) :-
	regex_kernel_rec(Regexp, Kernel0, AVL0, AVL1),
	kernel_normalize(Kernel0, Kernel),
	avl_store(Regexp, AVL1, Kernel, AVL).

regex_kernel_rec((R1\/R2), Kernel) --> !,
	regex_kernel(R1, K1),
	regex_kernel(R2, K2),
	{kernel_union(K1, K2, Kernel)}.
regex_kernel_rec((R1/\R2), Kernel) --> !,
	regex_kernel(R1, K1),
	regex_kernel(R2, K2),
	{kernel_intersection(K1, K2, Kernel)}.
regex_kernel_rec((R1\R2), Kernel) --> !,
	regex_kernel(R1, K1),
	regex_kernel(R2, K2),
	{kernel_difference(K1, K2, Kernel)}.
regex_kernel_rec(*(R1), Kernel) --> !,
	regex_kernel(R1, K1),
	{kernel_closure(K1, Kernel)}.
regex_kernel_rec(+(R1), Kernel) --> !,
	regex_kernel(R1, K1),
	{kernel_closure(K1, K2)},
	{kernel_concatenation(K1, K2, Kernel)}.
regex_kernel_rec(?(R1), Kernel) --> !,
	regex_kernel(R1 \/ [], Kernel).
regex_kernel_rec((R1+R2), Kernel) --> !,
	regex_kernel(R1, K1),
	regex_kernel(R2, K2),
	{kernel_concatenation(K1, K2, Kernel)}.
regex_kernel_rec({}, Kernel) --> !,
	{Kernel = kernel([],[])}.
regex_kernel_rec({Tree}, Kernel) --> !,
	{regex_unionify(Tree, Regexp)},
	regex_kernel(Regexp, Kernel).
regex_kernel_rec([], Kernel) --> !,
	{Kernel = kernel([source(S),sink(S)],[])}.
regex_kernel_rec([R1 | R2], Kernel) --> !,
	regex_kernel(R1, K1),
	regex_kernel(R2, K2),
	{kernel_concatenation(K1, K2, Kernel)}.
regex_kernel_rec(X, Kernel) -->
	{integer(X)}, !,
	{Kernel = kernel([source(S1),sink(S2)], [arc(S1,X,S2)])}.	

regex_unionify((X,Y), (R\/S)) :- !,
	regex_unionify(X, R),
	regex_unionify(Y, S).
regex_unionify(X, X).

kernel_closure(Kernel1, Closure) :-
	kernel_parts(Kernel1, Sources, Sinks, _, Arcs, _),
	kernel_tag_sources_sinks(Sources, Sources, Sinks1, Sources1),
	kernel_tag_sources_sinks([], Sinks, [], Sinks2),
	ord_union([Sinks1, Sinks2, Sources1], SS3),
	(   foreach(arc(Q3,A,Q4),Arcs),
	    fromto(Arcs1,Arcs2,Arcs6,[]),
	    param(Sinks,Sources)
	do  (   ord_member(Q4, Sinks)
	    ->  Arcs5 = [arc(Q3,A,Q4)|Arcs6],
		(   foreach(Q5,Sources),
		    fromto(Arcs2,Arcs3,Arcs4,Arcs5),
		    param(Q3,A)
		do  Arcs3 = [arc(Q3,A,Q5)|Arcs4]
		)
	    ;   Arcs2 = [arc(Q3,A,Q4)|Arcs6]
	    )
	),
	Closure = kernel(SS3,Arcs1).
	

kernel_complement(Kernel1, Complement) :-
	kernel_parts(Kernel1, Sources, Sinks, States, Arcs, _),
	Complement = kernel(SourcesSinks2,Arcs),
	ord_subtract(States, Sinks, NotSinks),
	kernel_tag_sources_sinks(Sources, NotSinks, Sources2, Sinks2),
	append(Sources2, Sinks2, SourcesSinks2).

kernel_intersection(Kernel1, Kernel2, Intersection) :-
	Intersection = kernel(SourcesSinks3,Arcs3),
	kernel_parts(Kernel1, Sources1, Sinks1, _, Arcs1, _),
	kernel_parts(Kernel2, Sources2, Sinks2, _, Arcs2, _),
	kernel_pairs(Sources1, Sources2, Sources3, []),
	kernel_closure1(Sources3, Sources3, Closure, Arcs1, Arcs2, Arcs3, []),
	kernel_pairs(Sinks1, Sinks2, Sinks3, []),
	ord_intersection(Sinks3, Closure, Sinks3c),
	kernel_tag_sources_sinks(Sources3, Sinks3c, SS1, SS2),
	append(SS1, SS2, SourcesSinks3).

kernel_union(Kernel1, Kernel2, Union) :-
	kernel_parts(Kernel1, Sources1, Sinks1, _, Arcs1, _),
	kernel_parts(Kernel2, Sources2, Sinks2, _, Arcs2, _),
	append(Sources1, Sources2, Sources12),
	append(Sinks1, Sinks2, Sinks12),
	append(Arcs1, Arcs2, Arcs12),
	kernel_tag_sources_sinks(Sources12, Sinks12, TSo12, TSi12),
	append(TSo12, TSi12, SS12),
	Union = kernel(SS12,Arcs12).

kernel_difference(Kernel1, Kernel2, Difference) :-
	kernel_parts(Kernel1, _, _, _, _    , Alpha1),
	kernel_parts(Kernel2, Sources2, Sinks2, _, Arcs2, Alpha2),
	kernel_tag_sources_sinks(Sources2, Sinks2, TSo2, TSi2),
	append(TSo2, TSi2, SS2),
	ord_union(Alpha1, Alpha2, Alpha3),
	ord_subtract(Alpha3, Alpha2, ToAdd2),
	(   foreach(A2,ToAdd2),
	    foreach(arc(_,A2,_),New2)
	do  true
	),
	append(Arcs2, New2, Arcs22),
	kernel_complement(kernel(SS2,Arcs22), K2C),
	kernel_intersection(Kernel1, K2C, Difference).

kernel_concatenation(Kernel1, Kernel2, Concat) :-
	kernel_parts(Kernel1, Sources1, Sinks1, _, Arcs1, _),
	kernel_parts(Kernel2, Sources2, Sinks2, _, Arcs2, _),
	Concat = kernel(SS3,Arcs3),
	kernel_tag_sources_sinks(Sources1, Sinks2, Sources3, Sinks3),
	(   foreach(arc(Q5,A5,R5),Arcs1),
	    fromto(New3,New4,New7,[]),
	    param(Sinks1,Sources2)
	do  (   ord_nonmember(R5, Sinks1) -> New4 = New7
	    ;   (   foreach(So5,Sources2),
		    fromto(New4,New5,New6,New7),
		    param(Q5,A5)
		do  New5 = [arc(Q5,A5,So5)|New6]
		)
	    )
	),
	(   ord_disjoint(Sources1, Sinks1) -> Sources4 = []
	;   kernel_tag_sources_sinks(Sources2, [], Sources4, [])
	),
	append([Sources3, Sources4, Sinks3], SS3),
	ord_union([Arcs1,Arcs2,New3], Arcs3).

%% rename states to brand new variables
%% if need be, add extra "black hole" state
%% ensure that every combo <state,letter> has at least one transition
%% output Sources, Sinks, States, Arcs, Alphabet as ordered sets
kernel_parts(Kernel1, Sources, Sinks, States, Arcs, Alphabet) :-
	kernel_rename_states(Kernel1, Kernel2),
	Kernel2 = kernel(SourcesSinks,Arcs1),
	(   foreach(Item,SourcesSinks),
	    fromto(Sources1,So1,So2,[]),
	    fromto(Sinks1,Si1,Si2,[]),
	    foreach(Y,Qs4)
	do  (   Item = source(Y) -> So1 = [Y|So2], Si1 = Si2
	    ;   Item = sink(Y)   -> So1 = So2, Si1 = [Y|Si2]
	    )
	),
	sort(Sources1, Sources),
	sort(Sinks1, Sinks),
	sort(Arcs1, Arcs2),
	(   foreach(arc(Q1,A1,Q2),Arcs2),
	    foreach(Q1*A1,Out1),
	    foreach(A1,As),
	    fromto(Qs1,Qs2,Qs3,Qs4)
	do  Qs2 = [Q1,Q2|Qs3]
	),
	sort(As, Alphabet),
	sort(Qs1, States1),
	sort(Out1, Out2),
	kernel_pairs(States1, Alphabet, Out3, []),
	ord_subtract(Out3, Out2, Out4),
	(   Out4 = [] -> Arcs = Arcs2, States = States1
	;   (   foreach(Q3*A3,Out4),
		foreach(arc(Q3,A3,Aux),Arcs3),
		param(Aux)
	    do  true
	    ),
	    (   foreach(A4,Alphabet),
		foreach(arc(Aux,A4,Aux),Arcs4),
		param(Aux)
	    do  true
	    ),
	    ord_union([Arcs2,Arcs3,Arcs4], Arcs),
	    ord_add_element(States1, Aux, States)
	).

kernel_rename_states(kernel(SourcesSinks1,Arcs1), kernel(SourcesSinks2,Arcs2)) :-
	kernel_rename_states(SourcesSinks1, SourcesSinks2, KL1, KL2),
	kernel_rename_states(Arcs1, Arcs2, KL2, []),
	keysort(KL1, KL3),
	keyclumped(KL3, KL4),
	(   foreach(_-Clump,KL4)
	do  (   foreach(X,Clump),
		param(X)
	    do  true
	    )
	).

kernel_rename_states(L1, L2) -->
	(   foreach(X,L1),
	    foreach(Y,L2)
	do  (   {X = source(Q1)}
	    ->  {Y = source(Q2)}, [Q1-Q2]
	    ;   {X = sink(Q1)}
	    ->  {Y = sink(Q2)}, [Q1-Q2]
	    ;   {X = arc(Q1,A,Q3)}
	    ->  {Y = arc(Q2,A,Q4)}, [Q1-Q2,Q3-Q4]
	    )
	).

kernel_tag_sources_sinks(Sources, Sinks, SS1, SS2) :-
	(   foreach(Q1,Sources),
	    foreach(source(Q1),SS1)
	do  true
	),
	(   foreach(Q2,Sinks),
	    foreach(sink(Q2),SS2)
	do  true
	).

kernel_pairs(Xs, Ys) -->
	(   foreach(X,Xs),
	    param(Ys)
	do  (   foreach(Y,Ys),
		param(X)
	    do  [X*Y]
	    )
	).

kernel_closure1([], Closure, Closure, _, _) --> [].
kernel_closure1([P1*P2|L1], Sofar1, Closure, Arcs1, Arcs2) -->
	{kernel_filter_arcs(Arcs1, P1, Arcs3)},
	{kernel_filter_arcs(Arcs2, P2, Arcs4)},
	{keyclumped(Arcs3, KL1)},
	{keyclumped(Arcs4, KL2)},
	(   foreach(A-Clump1,KL1),
	    fromto(Incr,S0,S6,[]),
	    param(KL2,P1,P2)
	do  (   foreach(B-Clump2,KL2),
		fromto(S0,S1,S5,S6),
		param(A,Clump1,P1,P2)
	    do  (   {A==B} ->
		    (   foreach(X,Clump1),
			fromto(S1,S2,S4,S5),
			param(A,Clump2,P1,P2)
		    do  (   foreach(Y,Clump2),
			    fromto(S2,[X*Y|S3],S3,S4),
			    param(A,P1,P2,X)
			do  [arc(P1*P2,A,X*Y)]
			)				      
		    )
		;   {S1 = S5}
		)
	    )
	),
	{sort(Incr, Incr1)},
	{ord_union(Sofar1, Incr1, Sofar2, L2)},
	{append(L1, L2, L3)},
	kernel_closure1(L3, Sofar2, Closure, Arcs1, Arcs2).

kernel_filter_arcs([], _, []).
kernel_filter_arcs([arc(P,A,Q)|Arcs], P1, KL) :-
	compare(K, P, P1),
	kernel_filter_arcs(K, A, Q, Arcs, P1, KL).

kernel_filter_arcs(<, _, _, Arcs, P1, KL) :-
	kernel_filter_arcs(Arcs, P1, KL).
kernel_filter_arcs(=, A, Q, Arcs, P1, [A-Q|KL]) :-
	kernel_filter_arcs(Arcs, P1, KL).
kernel_filter_arcs(>, _, _, _,    _,  []).

%% first, transform to DFA if need be
%% then, minimize
kernel_normalize(Kernel1, Kernel3) :-
	kernel_ensure_dfa(Kernel1, Kernel2),
	Kernel2 = kernel(SourcesSinks1,Arcs1),
	Kernel3 = kernel(SourcesSinks3,Arcs3),
	kernel_make_penta(SourcesSinks1, Arcs1, Penta1),
	kernel_remove_unreachable(Penta1, Penta2), 
	Penta2 = penta(States2,_,_,_,Sinks2),
	ord_subtract(States2, Sinks2, NonSinks2),
	(   Sinks2\==[], NonSinks2\==[] -> Partition0 = [NonSinks2,Sinks2]
	;   true -> Partition0 = [States2]
	),
	kernel_refine_partition(Partition0, Partition, Penta2),
	kernel_collapse(Penta2, Partition, Penta3),
	Penta3 = penta(_,_,ArcsF3,Sources3,Sinks3),
	avl_to_list(ArcsF3, ArcsL3),
	(   foreach((P-A)-Q,ArcsL3),
	    foreach(arc(P,A,Q),Arcs3)
	do  true
	),
	kernel_tag_sources_sinks(Sources3, Sinks3, SS1, SS2),
	append(SS1, SS2, SourcesSinks3),
	numbervars(Kernel3, 0, _).

kernel_make_penta(SourcesSinks, Arcs, penta(States,Alfabet,ArcsF,Sources,Sinks)) :-
	(   foreach(Item,SourcesSinks),
	    fromto(Sources0,So1,So2,[]),
	    fromto(Sinks0,Si1,Si2,[])
	do  (   Item = source(Y) -> So1 = [Y|So2], Si1 = Si2
	    ;   Item = sink(Y)   -> So1 = So2, Si1 = [Y|Si2]
	    )
	),
	(   foreach(arc(P,A,Q),Arcs),
	    foreach(A,Alfa0),
	    foreach((P-A)-Q,ArcsFL),
	    fromto(States0,[P,Q|S],S,[])
	do  true
	),
	sort(ArcsFL, ArcsFOL),
	ord_list_to_avl(ArcsFOL, ArcsF),
	sort(States0, States1),
	sort(Alfa0, Alfabet),
	sort(Sources0, Sources),
	sort(Sinks0, Sinks),
	ord_union([States1,Sources,Sinks], States).

kernel_remove_unreachable(Penta1, Penta2) :-
	Penta1 = penta(_,      Alfa,ArcsF1,Sources1,Sinks1),
	Penta2 = penta(States2,Alfa,ArcsF2,Sources2,Sinks2),
	avl_to_list(ArcsF1,ArcsFL),
	(   foreach((P-_)-Q,ArcsFL),
	    fromto(EdgesF1,[P-Q|EdgesF2],EdgesF2,AuxF),
	    fromto(EdgesB1,[Q-P|EdgesB2],EdgesB2,AuxB)
	do  true
	),
	(   foreach(So,Sources1),
	    fromto(AuxF,[(*)-So|AuxF1],AuxF1,[])
	do  true
	),
	(   foreach(Si,Sinks1),
	    fromto(AuxB,[(*)-Si|AuxB1],AuxB1,[])
	do  true
	),
	vertices_edges_to_ugraph([*], EdgesF1, GF),
	vertices_edges_to_ugraph([*], EdgesB1, GB),
	reachable(*, GF, ReachF),
	reachable(*, GB, ReachB),
	ord_intersection(ReachF, ReachB, ReachFB),
	ord_del_element(ReachFB, *, States2),
	ord_intersection(Sources1, States2, Sources2),
	ord_intersection(Sinks1, States2, Sinks2),
	(   foreach((P1-A1)-Q1,ArcsFL),
	    fromto(ArcsFL2,ArcsFL3,ArcsFL4,[]),
	    param(States2)
	do  (   ord_member(P1,States2),
		ord_member(Q1,States2) ->
		ArcsFL3 = [(P1-A1)-Q1|ArcsFL4]
	    ;   ArcsFL3 = ArcsFL4
	    )
	),
	ord_list_to_avl(ArcsFL2, ArcsF2).

kernel_refine_partition(Part0, Part, Penta) :-
	(   fromto(1,_,D,0),
	    fromto(Part0,Part1,Part2,Part),
	    param(Penta)
	do  kernel_refine_partition1(Part1, Part2, Penta),
	    length(Part1, N1),
	    length(Part2, N2),
	    D is N1-N2	
	).

kernel_refine_partition1(Part1, Part2, Penta) :-
	Penta = penta(_,Alfa,ArcsF,_,_),
	(   foreach(Part,Part1),
	    count(I,1,_),
	    fromto(AL,AL1,AL3,[])
	do  (   foreach(S,Part),
		fromto(AL1,[S-I|AL2],AL2,AL3),
		param(I)
	    do  true
	    )
	),
	sort(AL, AOL),
	ord_list_to_avl(AOL, Map),
	(   foreach(Q-J,AL),
	    foreach((J-SignSet)-Q,KL1),
	    param(Alfa,Map,ArcsF)
	do  (   foreach(A,Alfa),
		fromto(Sign,Sign1,Sign2,[]),
		param(Q,Map,ArcsF)
	    do  (   avl_fetch(Q-A, ArcsF, R) ->
		    avl_fetch(R, Map, R1),
		    Sign1 = [s(A,R1)|Sign2]
		;   Sign1 = Sign2
		)
	    ),
	    sort(Sign, SignSet)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(_-Clump,KL3),
	    foreach(Clump,Part2)
	do  true
	).

kernel_collapse(Penta1, Partition, Penta2) :-
	Penta1 = penta(States1,Alfa,Arcs1,Sources1,Sinks1),
	Penta2 = penta(States2,Alfa,Arcs2,Sources2,Sinks2),
	(   foreach(Part,Partition),
	    fromto(AL,AL1,AL3,[])
	do  (   foreach(S0,Part),
		fromto(AL1,[S0-SI|AL2],AL2,AL3),
		param(SI)
	    do  true
	    )
	),
	sort(AL, AOL),
	list_to_avl(AOL, Map),
	(   foreach(Q1,States1),
	    foreach(R1,States1b),
	    param(Map)
	do  avl_fetch(Q1, Map, R1)
	),
	avl_to_list(Arcs1, Arcs1L),
	(   foreach((P-A)-Q,Arcs1L),
	    foreach((R-A)-S,Arcs2L),
	    param(Map)
	do  avl_fetch(P, Map, R),
	    avl_fetch(Q, Map, S)
	),
	sort(Arcs2L, Arcs2OL),
	ord_list_to_avl(Arcs2OL, Arcs2),
	(   foreach(Q2,Sources1),
	    foreach(R2,Sources1b),
	    param(Map)
	do  avl_fetch(Q2, Map, R2)
	),
	(   foreach(Q3,Sinks1),
	    foreach(R3,Sinks1b),
	    param(Map)
	do  avl_fetch(Q3, Map, R3)
	),
	sort(States1b, States2),
	sort(Sources1b, Sources2),
	sort(Sinks1b, Sinks2).

/* NFA to DFA: standard powerset construction algorithm. */

kernel_ensure_dfa(Kernel1, Kernel2) :-
	kernel_parts(Kernel1, Sources, Sinks, _, Arcs, Alphabet),
	(   foreach(arc(Q1,A,Q2),Arcs),
	    foreach(Q1*A - Q2,KL1)
	do  true
	),
	keyclumped(KL1, KL2),
	(   Sources = [_,_|_] -> true
	;   member(_-[_,_|_], KL2) -> true
	), !,
	ord_list_to_avl(KL2, Trans),
	kernel_det_closure([Sources], Alphabet, Trans, [Sources], DStates, [], DArcs),
	DSources = [Sources],
	kernel_det_select(DStates, Sinks, DSinks),
	kernel_tag_sources_sinks(DSources, DSinks, ESources, ESinks),
	append(ESources, ESinks, ESS),
	Kernel2 = kernel(ESS,DArcs).
kernel_ensure_dfa(Kernel, Kernel).

kernel_det_closure([], _, _, States, States, Arcs, Arcs).
kernel_det_closure([R1|Queue], Alphabet, Trans, States0, States, Arcs0, Arcs) :-
	kernel_det_arcs(R1, Alphabet, Trans, Arcs2),
	sort(Arcs2, Arcs3),
	(   foreach(arc(_,_,R2),Arcs3),
	    foreach(R2,R2s)
	do  true
	),
	sort(R2s, R3s),
	ord_subtract(R3s, States0, New),
	ord_union(R3s, States0, States1),
	ord_union(Arcs0, Arcs3, Arcs1),
	append(Queue, New, Queue1),
	kernel_det_closure(Queue1, Alphabet, Trans, States1, States, Arcs1, Arcs).

kernel_det_arcs(R1, Alphabet, Trans, Arcs) :-
	(   foreach(A,Alphabet),
	    foreach(Arc,Arcs),
	    param(R1,Trans)
	do  (   foreach(Q1,R1),
		fromto(Qs1,Qs2,Qs3,[]),
		param(A,Trans)
	    do  avl_fetch(Q1*A, Trans, Q1As),
		append(Q1As, Qs3, Qs2)
	    ),
	    sort(Qs1, Qs4),
	    Arc = arc(R1,A,Qs4)
	).

kernel_det_select(All, Key, Selected) :-
	(   foreach(X,All),
	    fromto(Selected,Sel1,Sel2,[]),
	    param(Key)
	do  (   ord_disjoint(X, Key) -> Sel1 = Sel2
	    ;   Sel1 = [X|Sel2]
	    )
	).
	
kernel_string(Kernel, String) :-
	Kernel = kernel(SourcesSinks,Arcs),
	(   foreach(Item,SourcesSinks),
	    fromto(Init1,Init2,Init3,Init4),
	    fromto(Sinks1,Si1,Si2,[])
	do  (   Item = source(Y) -> Init2 = [Y-[]|Init3], Si1 = Si2
	    ;   Item = sink(Y)   -> Init2 = Init3, Si1 = [Y|Si2]
	    )
	),
	sort(Sinks1, Sinks),
	(   foreach(arc(Q2,A,Q3),Arcs),
	    foreach(Q2-(A-Q3),KL1)
	do  true
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	ord_list_to_avl(KL3, Map),
	kernel_string(Init1, Init4, Sinks, Map, String).

kernel_string(Head, Tail1, Sinks, Map, String) :-
	Head\==Tail1,
	Head = [State-Stack|Head1],
	(   ord_member(State, Sinks),
	    reverse(Stack, String)
	;   avl_fetch(State, Map, Clump)
	->  (   foreach(A-Q,Clump),
		fromto(Tail1,Tail2,Tail3,Tail4),
		param(Stack)
	    do  Tail2 = [Q-[A|Stack]|Tail3]
	    ),
	    kernel_string(Head1, Tail4, Sinks, Map, String)
	;   kernel_string(Head1, Tail1, Sinks, Map, String)
	).

