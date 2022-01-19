/* Copyright(C) 1997, Swedish Institute of Computer Science */
:- public
        solve_fd/2,
        'x=y'/3,
        'x\\=y'/3,
        'x=<y'/3,
        'x<y'/3,
        iff_aux/2,
        iff_aux/3,
        propagate_interval_chk/3.

:- meta_predicate
	fd_batch(:),
	fd_global(:,*,*),
	fd_global(:,*,*,*),
	fd_findall(*,:,*),
	gather_solutions(*,:,*,*,*).

%% see library/clpfd.pl
:- attribute fd_attribute/5.

fd_purge(X) :-
	var(X),
	get_atts(X, fd_attribute(_,_,_,_,_)), !,
	'$fd_purge'(X).
fd_purge(_).

purify_var(X) :-
	nonvar(X).
purify_var(X) :-
	var(X),
	put_atts(X, -fd_attribute(_,_,_,_,_)).

fd_var(X) :-
	var(X),
	is_fd_variable(X).

is_fd_variable(X) :-
	get_atts(X, fd_attribute(_,_,_,_,_)).

get_fd_domain(X, Dom) :-
	get_atts(X, fd_attribute(_,DomM,_,_,_)),
	get_mutable(Dom, DomM).

get_fd_suspensions(X, Lists) :-
	get_atts(X, fd_attribute(_,_,ListsM,_,_)),
	get_mutable(Lists, ListsM).

get_fd_aliases(X, Aliases) :-
	get_atts(X, fd_attribute(_,_,_,AliasesM,_)),
	get_mutable(AliasesOpen, AliasesM),
	close_list(AliasesOpen, Aliases).

get_fd_failures(X, Failures) :-
	get_atts(X, fd_attribute(_,_,_,_,Failures)).

put_fd_failures(X, Failures) :-
	get_atts(X, fd_attribute(A,B,C,D,_)),
	put_atts(X, fd_attribute(A,B,C,D,Failures)).

close_list(Tail, Nil) :- var(Tail), !, Nil = [].
close_list([X|Y], [X|Z]) :-
	close_list(Y, Z).

solve_fd(Constraint, DefPtr) :-
	check_arguments(Constraint, Attv),
	iff_aux(DefPtr, Constraint, Attv, 1).

%%% Reified constraints, accelerated in 4.0.5

'x=y'(X, Y, B) :-
	integer(X),
	integer(Y),
	X=:=Y, !,
	B = 1.
'x=y'(X, Y, B) :-
	fd_minmax(X, Xmin, Xmax),
	fd_minmax(Y, Ymin, Ymax),
	(   Xmax < Ymin -> B = 0
	;   Xmin > Ymax -> B = 0
	), !.
'x=y'(X, Y, B) :-
	iff_aux('t=u IND'(X,Y), clpfd, B).

'x\\=y'(X, Y, B) :-
	integer(X),
	integer(Y),
	X=:=Y, !,
	B = 0.
'x\\=y'(X, Y, B) :-
	fd_minmax(X, Xmin, Xmax),
	fd_minmax(Y, Ymin, Ymax),
	(   Xmax < Ymin -> B = 1
	;   Xmin > Ymax -> B = 1
	), !.
'x\\=y'(X, Y, B) :-
	iff_aux('x\\=y IND'(X,Y), clpfd, B).

'x=<y'(X, Y, B) :- 
	fd_minmax(X, Xmin, Xmax),
	fd_minmax(Y, Ymin, Ymax),
	(   Xmax =< Ymin -> B = 1
	;   Xmin  > Ymax -> B = 0
	), !.
'x=<y'(X, Y, B) :- 
	iff_aux('x=<y IND'(X,Y), clpfd, B).

'x<y'(X, Y, B) :-
	fd_minmax(X, Xmin, Xmax),
	fd_minmax(Y, Ymin, Ymax),
	(   Xmax <  Ymin -> B = 1
	;   Xmin >= Ymax -> B = 0
	), !.
'x<y'(X, Y, B) :-
	iff_aux('x<y IND'(X,Y), clpfd, B).

% accelerated version for finite bounds, merely fail if error
fd_minmax(X, Min, Max) :-
	var(X), !,
	get_fd_domain(X, Dom),
	Dom = dom(_,Min,Max,_),
	integer(Min),
	integer(Max).
fd_minmax(X, Min, Max) :-
	integer(X),
	Min = X,
	Max = X.

iff_aux(Constraint0, B) :-
	prolog:get_module_meta(Constraint0, Constraint, Module),
	iff_aux(Constraint, Module, B).

iff_aux('x=y'(X,Y), clpfd, B) :- !, % obsolescent
	'x=y'(X, Y, B).
iff_aux('x\\=y'(X,Y), clpfd, B) :- !, % obsolescent
	'x\\=y'(X, Y, B).
iff_aux('x=<y'(X,Y), clpfd, B) :- !, % obsolescent
	'x=<y'(X, Y, B).
iff_aux('x<y'(X,Y), clpfd, B) :- !, % obsolescent
	'x<y'(X, Y, B).
iff_aux(scalar_product(Cs,Xs,R,Y), _, B) :- !, % since 4.5
	scalar_product_reif(Cs, Xs, R, Y, B).
iff_aux(scalar_product(Cs,Xs,R,Y,Opt), _, B) :- !, % since 4.5
	scalar_product_reif(Cs, Xs, R, Y, B, Opt).
iff_aux(all_equal(Xs), _, B) :- !, % since 4.7
	all_equal_reif(Xs, B).
iff_aux(element(I,L,Y), M, B) :- !,
	trans_bool(element(I,L,Y) #<=> B, M, Goals, []),
	(   foreach(Goal,Goals)
	do  call(clpfd:Goal)
	).
iff_aux(table(Tuples,Ext), M, B) :- !,
	trans_bool(table(Tuples,Ext) #<=> B, M, Goals, []),
	(   foreach(Goal,Goals)
	do  call(clpfd:Goal)
	).
iff_aux(X in Expr, _, B) :- !,
        fd_goal_expansion(X in Expr #<=> B, clpfd, Goal),
	call(Goal).
iff_aux(Constraint, _, B) :-
	fd_expandable(Constraint, _, _, _), !,
	fd_goal_expansion(Constraint #<=> B, clpfd, Goal),
	call(Goal).
iff_aux(Constraint, Module, B) :-
	'$fd_find_definition'(Constraint, Module, DefPtr),
	(   DefPtr =\= 0
	->  check_arguments(Constraint, Attv),
	    iff_aux(DefPtr, Constraint, Attv, B)
	;   functor(Constraint, Name, Arity),
	    illarg(existence(constraint,Module:Name/Arity,0), Constraint, 0)
	).

iff_aux(Def, Constraint, Attv, B) :-
	'$fd_in_interval'(B, 0, 1, 1), !,
	arg_attribute(B, Ba, Constraint, 1),
	'$fd_post_reified'(Def, Constraint, Attv, B, Ba),
	% print_message(warning, post_indexical(Constraint,B)), % MC trace
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
iff_aux(_, Constraint, _, B) :- % raise error, or fail
	arg_attribute(B, _, Constraint#<=>B, 2),
	fail.

check_arguments(Constraint, Attv) :-
	'$fd_check_arguments'(Constraint, Attv), !.
check_arguments(Constraint, _) :-
	functor(Constraint, _, A),
	check_arguments_error(A, Constraint).

check_arguments_error(0, _) :- !.
check_arguments_error(A, Constraint) :-
	arg(A, Constraint, Arg),
	(   var(Arg)
	;   integer(Arg),
	    \+prolog:'$large_data'(0, Arg, _)
	), !,
	B is A-1,
	check_arguments_error(B, Constraint).
check_arguments_error(A, Constraint) :-
	arg(A, Constraint, Arg),
	fd_argument_error(Constraint, A, Arg).

fd_argument_error(Constraint, A, Arg) :-
	integer(Arg), !,
	'$fd_overflow'(error, error, 1),
	(Arg<0 -> What=min_clpfd_integer ; What=max_clpfd_integer),
	illarg(representation(What), Constraint, A, Arg).
fd_argument_error(Constraint, A, Arg) :-
	illarg(type(integer), Constraint, A, Arg).

/* now unfolded
evaluate :-
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
*/
%% evaluate(RC, Global)
%% RC = -1 -- failure
%% RC = 0  -- done
%% RC = 1  -- indexicals to be run
%% RC = 2  -- Global to be run
evaluate(0, _).
evaluate(1, _) :-
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
evaluate(2, Propagator) :-
	dispatch_prune_and_enqueue(Propagator).

% FDBG puts advice on this!
prune_and_propagate(Pruned, Set) :-
	'$fd_in_set'(Pruned, Set, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
prune_and_propagate_chk(Pruned, Set) :-
	'$fd_in_set'(Pruned, Set, 1), !, % SPRM 12637
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
prune_and_propagate_chk(Pruned, Set) :-
	arg_attribute(Pruned, _, in_set(Pruned,Set), 1), !,
	fail.
prune_and_propagate_chk(_, _).

% FDBG puts advice on this!
propagate_interval(Pruned, Min, Max) :-
	'$fd_in_interval'(Pruned, Min, Max, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

% FDBG puts advice on this!
propagate_interval_chk(Pruned, Min, Max) :-
	'$fd_in_interval'(Pruned, Min, Max, 1), !, % SPRM 12637
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).
propagate_interval_chk(Pruned, Min, Max) :-
	arg_attribute(Pruned, _, in(Pruned,Min..Max), 1), !,
	fail.
propagate_interval_chk(_, _, _).

/* Unifying domain variables, using alias mechanism, since SP 4.5.1. */

% FDBG puts advice on this!
verify_attributes(Var, Other, Goals) :-
	Constraint = (Var = Other),
	check_arguments(Constraint, _Attv),
	Internal = 't=u+c'(Var,Other,0),
	check_arguments(Internal, Attv),
	Goals = [equate_and_propagate(Internal, Attv)].

fd_unify(X, Y) :-
	'$fd_unify'(X, Y, RC),
	evaluate(RC, 0).

%%% Support for global (specialized) constraints.

% Susp is a list of F(Var) terms, each representing that the constraint should be
% suspended on the variable Var.  F denotes the circumstances under which the constraint
% should be resumed:
% dom - resume when dom(Var) changes
% min - resume when min(Var) changes
% max - resume when max(Var) changes
% minmax - resume when min(Var) or max(Var) changes
% val - resume when Var becomes nonvar
fd_global(ModConstraint, State, Susp) :-
	fd_global(ModConstraint, State, Susp, []).

fd_global(ModConstraint, State, Susp, Options) :-
	Goal = fd_global(ModConstraint, State, Susp, Options),
	must_be(Options, proper_list, Goal, 4),
	fd_global_options(Options, opt(_,ModConstraint,0), Opt, Goal, 4),
	prolog:get_module_meta_arg(ModConstraint, Constraint, _Module),
	Opt = opt(Glob,Source,Base),
	fd_global_internal(Constraint, State, Susp, Glob, Source, Base).

fd_global_options([], Opt, Opt, _, _).
fd_global_options([X|L], Opt0, Opt, Goal, ArgNo) :-
	(   nonvar(X),
	    fd_global_option(X, Opt0, Opt1) -> true
        ;   illarg(domain(term,fd_global_option), Goal, ArgNo, X)
        ),
	fd_global_options(L, Opt1, Opt, Goal, ArgNo).

fd_global_option(global(Glob), opt(_,Source,Base), opt(Glob,Source,Base)).
fd_global_option(source(Source), opt(Glob,_,Base), opt(Glob,Source,Base)).
fd_global_option(idempotent(Bool), opt(Glob,Source,_), opt(Glob,Source,Base)) :-
	bool_option(Bool, B),
	Base is (1-B)<<2.

fd_global_internal(Constraint, State, Susp, Global, Source, Base) :-
	(   \+'$fd_coref'(Susp) -> Status is Base
	;   Status is Base\/4
	),
	'$fd_begin',
	'$fd_post_global'(Constraint, State, Status, Source, Susp, Global),
	% print_message(warning, post_global(Constraint)), % MC trace
	'$fd_evaluate_indexical'(RC, Global1),
	evaluate(RC, Global1).

dispatch_prune_and_enqueue(Indexical) :-
	Indexical = ix(_Ptr,Constraint,_StatusM,_Ent,_ZeroOne,_Attv,_LAttr,_Source), !,
	'$fd_eval_indexical'(Constraint, Actions, Indexical),
	enqueue_actions(Actions, Actions1),
	'$fd_prune_and_enqueue'(Actions1, Indexical), % at most one action
	'$fd_evaluate_indexical'(RC, Global1),
	evaluate(RC, Global1).
dispatch_prune_and_enqueue(Global) :-
	Global = global(StateM,Constraint,_StatusM,_Ent,_Atts,_Module), !,
	get_mutable(State, StateM),
	dispatch_global_fast(Constraint, State, NewState, Actions, Global),
	update_mutable(NewState, StateM),
	enqueue_actions(Actions, Actions1),
	'$fd_prune_and_enqueue'(Actions1, Global), % at most one action
	'$fd_evaluate_indexical'(RC, Global1),
	evaluate(RC, Global1).

enqueue_actions([], []).
enqueue_actions([X=I|Actions0], Actions) :-
	'$fd_range'(I, I, Set, 1), !,
	'$fd_in_set'(X, Set, 0),
	enqueue_actions(Actions0, Actions).
enqueue_actions([X=I|_], _) :- !,
	fd_argument_error(X=I, 2, I).
enqueue_actions([X in R|Actions0], Actions) :- !,
	set_expression_check(R, S, X in R, 2),
	'$fd_in_set'(X, S, 0),
	enqueue_actions(Actions0, Actions).
enqueue_actions([X in_set S|Actions0], Actions) :- !,
	(   '$fd_size'(S, _, 1) ->
	    '$fd_in_set'(X, S, 0)
	;   illarg(domain(term,constraint), X in_set S, 2)
	),
	enqueue_actions(Actions0, Actions).
enqueue_actions([call(Goal)|Actions0], Actions) :- !,
	call(user:Goal),	% default module, for lack of a better way
	enqueue_actions(Actions0, Actions).
enqueue_actions([A|Actions0], [A|Actions]) :-
	enqueue_actions(Actions0, Actions).

% clpfd:dispatch_global(+Constraint, +State0, -State, -Actions)
% calls a user-defined constraint solver for a particular kind of constraint.
% Constraint is the original constraint;
% State0 is a term representing aux. info about this constraint at the time of its
% latest invocation.
% State represents updated aux. info;
% Actions is a list of terms of the following form:
%	exit - the constraint has been found entailed
%	fail - the constraint has been found disentailed
%	call(G) - call the Prolog goal G
%	X = R   - call(X = R)
%	X in R   - call(X in R)
%	X in_set R  - call(X in_set R)

% FDBG puts advice on this!
dispatch_global_fast(Constraint, State, NewState, Actions, Global) :-
	'$fd_dispatch_global_fast'(Constraint, State, NewState1, Actions1, Global, RC),
	(   RC =:= 1
	->  NewState = NewState1,
	    Actions = Actions1
	;   dispatch_global(Constraint, State, NewState, Actions)
	).

:- multifile
	dispatch_global/4.

fd_batch(M:Goals) :-
	must_be(Goals, proper_list(callable), fd_batch(Goals), 1),
	'$fd_begin',
	'$fd_batch'(Old, on, 1),
	call_cleanup(post_and_eval(Goals,M,Old), '$fd_batch'(_,Old,1)).

post_and_eval(Goals, M, OnOff) :-
	(   foreach(Goal,Goals),
	    param(M)
	do  call(M:Goal)
	), !,
	'$fd_batch'(_, OnOff, 1),
	'$fd_evaluate_indexical'(RC, Global),
	evaluate(RC, Global).

/* fd_closure/2 */

% Given a list Vars of domain variables, Closure is the set
% of variables (including Vars) that can be transitively reached
% via constraints. Thus, fd_closure/2 is the transitive closure
% of fd_neighbors/2.
fd_closure(Vars, Closure) :-
	sort(Vars, Open),
	fd_closure(Open, Open, Closure).

%% new logic, based on copy_term/3, is faster if same constraint is attached to many vars
fd_closure([], Closure, Closure) :- !.
fd_closure(Open1, Closed, Closure) :-
	(   foreach(X,Open1),
	    foreach(Susps,Suspss)
	do  all_suspensions(X, Susps)
	),
	term_variables_set(Suspss, New),
	ord_union(Closed, New, Closed1, Open2),
	fd_closure(Open2, Closed1, Closure).

/* fd_neighbors/2 */

% Given a domain variable Var, Neighbors is the set
% of other variables, domain or otherwise, that occur with Var in some constraint.
fd_neighbors(Var, Neighs) :-
	var(Var),
	all_suspensions(Var, Susps), !,
	prolog:term_variables_set(Susps, Vars),
	ord_del_element(Vars, Var, Neighs).
fd_neighbors(_, []).

/* fd_flag(+Flag, ?Old, ?New)
   Flag=debug -> Old,New in [off,on]
   Flag=overflow -> Old,New in [error,fail]
*/
fd_flag(Flag, Old, New) :-
	Goal = fd_flag(Flag,Old,New),
	must_be(Flag, oneof([debug,overflow]), Goal, 1),
	fd_flag(Flag, Old, New, Goal).

fd_flag(debug, Old, New, Goal) :-
	(   New==Old -> true
	;   must_be(New, oneof([on,off]), Goal, 3)
	),
	'$fd_debug'(Old, New,1).
fd_flag(overflow, Old, New, Goal) :-
	(   New==Old -> true
	;   must_be(New, oneof([error,fail]), Goal, 3)
	),
	'$fd_overflow'(Old, New,1).

fd_hiding(Goal) :-
	'$fd_hiding'(Old, on, 1),
	call_cleanup(clpfd:Goal, '$fd_hiding'(_,Old,1)).

/* fd_findall(+Template, :Generator, -Solutions)
   Special case of findall/3.
   No evidence yet that it's worth it.
*/

fd_findall(Template, Generator, Out) :-
	Goal = fd_findall(Template, Generator, Out),
	must_be(Template, proper_list, Goal, 1),
	prolog:choice,		% see Bips/setof.pl for rationale
	'$fd_findall_begin'(Root, Template),
	call_cleanup(gather_solutions(Template, Generator, List, Root, Goal),
		     '$fd_findall_end'(Root)), !,
	Out = List.

gather_solutions(Template, Generator, _, Root, Goal) :-
	call(Generator),
	'$fd_findall_inserta'(Root, Template, RC),
	gather_solutions_error(RC, Template, Goal).
gather_solutions(_, _, List, Root, _) :-
	'$fd_findall_list'(Root, List).

gather_solutions_error(1, Template, Goal) :-
	must_be(Template, proper_list(integer), Goal, 1),
	must_be_dvar_list(Template, Goal, 1).

