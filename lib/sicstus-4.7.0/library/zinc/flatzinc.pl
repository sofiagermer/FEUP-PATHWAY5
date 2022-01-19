/* Copyright(C) 2008, Swedish Institute of Computer Science */

%   File         : flatzinc.pl
%   Author       : agren
%   Purpose      : Predicates to handle FlatZinc 1.6 programs.
%   LIMITATIONS  : - Only finite domain *integer* variables are supported.

:- module(flatzinc, [fzn_dump/2,
		     fzn_dump/3,
		     fzn_identifier/3,
		     fzn_load_file/2,
		     fzn_load_stream/2,
		     fzn_objective/2,
		     fzn_output/1, 
		     fzn_post/1,
		     fzn_run_file/1,
		     fzn_run_file/2,
		     fzn_run_stream/1,
		     fzn_run_stream/2,
		     fzn_run_stream/3,
		     fzn_solve/1]).

:- use_module(library(mutdict), [
	new_mutdict/1,
	mutdict_get/3,
	mutdict_put/3
	]).
:- use_module(library(clpfd)).
:- use_module(library(codesio), [
	format_to_codes/3
	]).
:- use_module(library(lists), [
	append/2,
	append_length/4,
	keyclumped/2,
	max_member/2,
	min_member/2,
	nth0/3,
	nth1/3,
	select/3,
	selectchk/3,
	transpose/2
	]).
:- use_module(library(ordsets), [
	ord_add_element/3,
	ord_subtract/3
	]).
:- use_module(library(process), [
	process_create/3
	]).
:- use_module(library(random), [
	random_member/2
	]).
:- use_module(library(structs), [
	dispose/1,
	get_contents/3,
	new/2,
	put_contents/3
	]).
:- use_module(library(terms), [
	term_hash/2,
	term_variables_bag/2,
	term_variables_set/2
	]).
:- use_module(library(types), [
	illarg/3
	]).
:- use_module(library('zinc/zinc_utils')).

:- load_files(library(str_decl), [when(compile_time), if(changed)]).

:- meta_predicate(error_handler(0,?)).
:- meta_predicate(warning(+,:)).
:- meta_predicate(my_module(:)).

% [PM] 4.2.1 return module context of caller
my_module(M:Module) :- Module = M.

%%%%% Top-level %%%%%

fzn_run_stream(Stream) :-
	fzn_run_stream(Stream, [], fzn_run_stream(Stream)-0).

fzn_run_stream(Stream, Options) :-
	fzn_run_stream(Stream, Options, fzn_run_stream(Stream, Options)-2).

fzn_run_stream(Stream, Options, GoalArg) :-
	zinc_options(Options, GoalArg, Options1),
	(   memberchk(ozn_file(OznBase), Options1)
	->  Goal-_ = GoalArg,
	    tool_path(minizinc, 'MINIZINC', MiniZinc, Goal),
	    zinc_open(OznBase, read, '.ozn', OznStream),
	    stream_property(OznStream, file_name(OznPath)),
	    close(OznStream),
	    OznFileArgs = ['--ozn-file',file(OznPath)], % These args make it emulate solns2out
	    (   memberchk(output(OutFile), Options1)
	    ->  Args = ['-o',file(OutFile)|OznFileArgs]
	    ;   Args = OznFileArgs
	    ),
	    process_create(MiniZinc, Args, [stdin(pipe(SolnsStream)),
					    process(Solns2OutProc)]),
	    set_output(SolnsStream)
	;   memberchk(output(OutFile), Options1)
	->  open(OutFile, write, OutStream),
	    set_output(OutStream)
	;   true
	),
	(   memberchk(solutions(Solutions), Options1) -> true
	;   Solutions = 1
	),
	(   memberchk(search(Search), Options1) -> true
	;   Search = bab
	),
	(   memberchk(free_search(FSearch), Options1) -> true
	;   FSearch = false
	),
	(   memberchk(timeout(Ms), Options1) ->
	    TO = [time_out(Ms,_)]
	;   TO = []
	),
	fzn_new_statistics(Options1, Statistics0),
	fzn_set_statistics(Statistics0),
	fzn_new_statistics(Options1, Statistics1),
	fzn_new_statistics(Options1, Statistics2),
	fzn_new_state(State0),
	new_integer(0, Found),
	state_set([found,solutions,search,free_search,time_out], State0, [Found,Solutions,Search,FSearch,TO], State1),
	call_cleanup(fzn_run_stream_call(Options1, Stream, State1, Statistics0, Statistics1, Statistics2, Found),
		     fzn_run_stream_cleanup(Found, Statistics0, Statistics1, Statistics2, SolnsStream, Solns2OutProc, OutStream)).

fzn_run_stream_cleanup(Found, Statistics0, Statistics1, Statistics2, SolnsStream, Solns2OutProc, OutStream) :-
	dispose(Found),
	fzn_dispose_statistics(Statistics0),
	fzn_dispose_statistics(Statistics1),
	fzn_dispose_statistics(Statistics2),
	safe_close(SolnsStream),
	safe_process_wait(Solns2OutProc, _),
	safe_close(OutStream).

fzn_run_stream_call(Options1, Stream, State1, Statistics0, Statistics1, Statistics2, Found) :-
	fzn_run_stream_goal(Stream, State1, State, Statistics1, Exhausted),
	fzn_set_statistics(Statistics2),
	integer_value(Found, FoundV),
	write_exhausted(Exhausted, FoundV),
	state_get([vars], State, [Vars]),
	length(Vars, NVars),
	write_statistics(Options1, Statistics0, Statistics1, Statistics2, FoundV, Exhausted, NVars).

fzn_run_stream_goal(Stream, State1, State, Statistics1, Exhausted) :-
	call_cleanup(error_handler(interpret_items(Stream, State1, State), Stream),
		     fzn_set_statistics(Statistics1)),
	(   fzn_post(State)
	->  fzn_all_solutions(State, Exhausted)
	;   Exhausted = true
	).

fzn_new_statistics(Options, Statistics) :-
	memberchk(statistics(true), Options), !,
	new_integer(0, IT),
	new_integer(0, IP),
	new_integer(0, IB),
	new_integer(0, IC),
	Statistics = fzn_statistics(IT, IP, IB, IC).
fzn_new_statistics(_, fzn_statistics).

fzn_set_statistics(fzn_statistics(IT, IP, IB, IC)) :-
	statistics(runtime, [T|_]),
	fd_statistics(resumptions, P),
	fd_statistics(backtracks, B),
	fd_statistics(constraints, C),
	put_contents(IT, contents, T),
	put_contents(IP, contents, P),
	put_contents(IB, contents, B),
	put_contents(IC, contents, C).
fzn_set_statistics(fzn_statistics).

fzn_statistics_values(fzn_statistics(IT, IP, IB, IC),
		      Time, Resumptions, Backtracks, Constraints) :-
	integer_value(IT, Time),
	integer_value(IP, Resumptions),
	integer_value(IB, Backtracks),
	integer_value(IC, Constraints).

fzn_dispose_statistics(fzn_statistics).
fzn_dispose_statistics(fzn_statistics(IT, IP, IB, IC)) :-
	dispose(IT),
	dispose(IP),
	dispose(IB),
	dispose(IC).

write_exhausted(true, 0) :- !,
	write('=====UNSATISFIABLE====='), nl.
write_exhausted(true, _) :- !,
	write('=========='), nl.
write_exhausted(time_out, 0) :- !,
	write('=====UNKNOWN====='), nl.
write_exhausted(_, _).

write_statistics(Options, S0, S1, S2, SolutionsFound, Exhausted, NVars) :-
	memberchk(statistics(true), Options), !,
	nl,
	(   Exhausted = time_out
	->  print_message(warning, time_out)
	;   true
	),
	fzn_statistics_values(S0, T0, _, _, _),
	fzn_statistics_values(S1, T1, _, _, _),
	fzn_statistics_values(S2, T2, Resumptions, Backtracks, Constraints),
	RunTime is T2 - T0,
	SolveTime is T2 - T1,
	format('%%%mzn-stat: initTime=~t~16+~g\n', [(RunTime-SolveTime)/1000]),
	format('%%%mzn-stat: solveTime=~t~16+~g\n', [SolveTime/1000]),
	format('%%%mzn-stat: solutions=~t~16+~d\n', [SolutionsFound]),
	format('%%%mzn-stat: failures=~t~16+~d\n', [Backtracks]),
	format('%%%mzn-stat: variables=~t~16+~d\n', [NVars]),
	format('%%%mzn-stat: propagators=~t~16+~d\n', [Constraints]),
	format('%%%mzn-stat: propagations=~t~16+~d\n', [Resumptions]),
	format('%%%mzn-stat-end\n', []).
write_statistics(_, _, _, _, _, _, _).

:- fzn_all_solutions/2 is nondet.
fzn_all_solutions(State, Exhausted) :- 
	state_get([found,solve,solutions,time_out], State, [Found,Solve,Solutions,TO]),
	(fzn_get_objective(Solve,_) -> MinMax = true ; MinMax = false),
	fzn_solve(State),
	(TO = [time_out(_,TOFlag)] -> true ; TOFlag = success),
	(   TOFlag = time_out
	->  Exhausted = time_out
	;   TOFlag = optimality
	->  Exhausted = true
	;   MinMax = true, TO\==[], Solutions\==all
	->  Exhausted = time_out
	;   MinMax = true, integer(Solutions)
	->  Exhausted = true
	;   integer_value(Found, S),
	    S == Solutions,
	    Exhausted = false
	), !.
fzn_all_solutions(_, true).

new_integer(Value, Integer) :-
	new(integer, Integer),
	put_contents(Integer, contents, Value).

integer_value(Counter, V) :-
	get_contents(Counter, contents, V).

fzn_run_file(File) :-
	fzn_run_file(File, [], fzn_run_file(File)-0).

fzn_run_file(File, Options) :-
	fzn_run_file(File, Options, fzn_run_file(File, Options)-2).

fzn_run_file(File, Options, GoalArg) :-
	zinc_open(File, read, '.fzn', Stream),
	call_cleanup(fzn_run_stream(Stream, Options, GoalArg),
		     close(Stream)).

fzn_load_stream(Stream, State) :-
	fzn_new_state(State0),
	error_handler(interpret_items(Stream, State0, State), Stream).

fzn_new_state(State) :-
	new_mutdict(Table),
	State = state([table-Table,
		       vars-[],
		       output_vars-[],
		       model-[],
		       search-bab,  % 4.3
		       free_search-false,  % 4.7.0
		       solutions-1, % 4.3
		       deferred-[], % 4.3
		       solve-false,
		       time_out-[], % 4.4
		       found-[]]).

fzn_load_file(File, State) :-
	zinc_open(File, read, '.fzn', Stream),
	call_cleanup(fzn_load_stream(Stream, State), close(Stream)).

fzn_dump(State, File) :-
	fzn_dump(State, [], File).

fzn_dump(State, Options0, File) :-
	zinc_options(Options0, fzn_dump(State, Options0, File)-2, Options),
	state_get([model], State, [Model]),
	fzn_variables(Options, State, OutVars0),
	(   OutVars0 = []
	->  state_get([vars], State, [VarBag]),
	    sort(VarBag, VarSet),
	    OutVars = [vars=VarSet]
	;   OutVars = OutVars0
	),
	zinc_open(File, write, '.pl', Stream),
	format(Stream, ':- use_module(library(clpfd)).~n~n', []),
	sort_goals(dump, Model, Sorted),
	andify(Sorted, Goal),
	call_cleanup(portray_clause(Stream, (query(OutVars) :- Goal)),
		     close(Stream)).

fzn_variables([], _, []).
fzn_variables([variables(Vars)|Os], State, OutVars) :- !,
	bind_variables(Vars, State, OutVars0),
	fzn_variables(Os, State, OutVars1),
	append(OutVars0, OutVars1, OutVars).
fzn_variables([_|Os], State, OutVars) :-
	fzn_variables(Os, State, OutVars).

bind_variables([], _, []).
bind_variables([Id=Var|Vars], State, [Id=Var|OutVars]) :-
	fzn_identifier(State, Id, Var),
	bind_variables(Vars, State, OutVars).

fzn_post(State) :-
	state_get([model,solve], State, [Model,SolveGoal]),
	SolveGoal = (solve(_,SubGoals),_),
	term_variables_bag(SubGoals, Vars),
	term_variables_bag(Model, MVars),
	prolog_flag(min_tagged_integer, Minint),
	prolog_flag(max_tagged_integer, Maxint),
	(foreach(X,MVars) do clpfd:purify_var(X)),
	sort_goals(post, Model, Sorted),
	fd_batch([domain(Vars, Minint, Maxint) | Sorted]),
	garbage_collect.

sort_goals(dump, Goals, Sorted) :-
	(   foreach(Goal,Goals),
	    fromto(Sorted,Dom2,Dom3,Ctr1),
	    fromto(Ctr1,Ctr2,Ctr3,[])
	do  (   Goal = (_ in_set _)
	    ->  Dom2 = [Goal | Dom3], Ctr2 = Ctr3
	    ;   Goal = domain(_,_,_)
	    ->  Dom2 = [Goal | Dom3], Ctr2 = Ctr3
	    ;   Dom2 = Dom3, Ctr2 = [Goal | Ctr3]
	    )
	).
sort_goals(post, Goals, Sorted) :-
	(   foreach(Goal,Goals),
	    fromto(InSet1,InSet2,InSet3,[]),
	    fromto(Dom1,Dom2,Dom3,Ctr1),
	    fromto(Ctr1,Ctr2,Ctr3,[])
	do  (   Goal = (X #= Y), var(X), simple(Y)
	    ->  Dom2 = Dom3, InSet2 = InSet3, Ctr2 = Ctr3, X = Y
	    ;   Goal = (Var in_set Set)
	    ->  Dom2 = Dom3, InSet2 = [Set-Var | InSet3], Ctr2 = Ctr3
	    ;   Goal = domain(_,_,_)
	    ->  Dom2 = [Goal | Dom3], InSet2 = InSet3, Ctr2 = Ctr3
	    ;   Dom2 = Dom3, InSet2 = InSet3, Ctr2 = [Goal | Ctr3]
	    )
	),
	keysort(InSet1, KL1),
	keyclumped(KL1, KL2),
	(   foreach(Set1-Clump,KL2),
	    fromto(Sorted,Sorted2,Sorted3,Dom1)
	do  Sorted2 = [clpfd:domain(Clump,Set1) | Sorted3]
	).

:- fzn_solve/1 is nondet.
fzn_solve(State) :-
	state_get([solve], State, [Solve]),
	call(Solve).

fzn_output(State) :-
	\+fzn_has_timed_out(State), !,
	state_get([found], State, [Counter]),
	(   Counter = [] -> true
	;   get_contents(Counter, contents, V0),
	    V is V0 + 1,
	    put_contents(Counter, contents, V)
	),
	output_goal(State, Goal),
	call(Goal),
	flush_output.		% we want to see solutions ASAP
fzn_output(_).

fzn_has_timed_out(State) :-
	state_get([time_out], State, [[time_out(_,time_out)]]).

output_goal(State, Goal) :-
	state_get([table, output_vars], State, [Table, OutputVars]),
	(   foreach(Id-IndexRanges, OutputVars),
	    foreach(Output, GoalList0),
	    param(Table)
	do  expr_to_codes(Id, Table, ExprCodes),
	    (   IndexRanges = []
	    ->  Output = format("~a = ~s;\n", [Id, ExprCodes])
	    ;   (   foreach(IR, IndexRanges),
		    foreach(IRC, IndexRangesCodes),
		    foreach(F, RangesFormat),
		    param(Table)
		do  expr_to_codes(IR, Table, IRC),
		    F = "~s, "
		),
		length(IndexRanges, N),
		append(["~a = array~dd("|RangesFormat], Format0),
		append(Format0,  "~s);\n", Format),
		append([Id, N|IndexRangesCodes], [ExprCodes], Args),
		Output = format(Format, Args)
	    )
	),
	append(GoalList0, [write('----------\n')], GoalList),
	andify(GoalList, Goal).

fzn_identifier(State, Id, Value) :-
	state_get([table], State, [Table]),
	table_lookup_value(Id, Table, Value0),
	deref(Value0, Table, Value1),
	fzn_to_prolog(Value1, Value2),
	Value = Value2.

fzn_to_prolog(X, X) :-
	var(X), !.
fzn_to_prolog(Array, Values) :-
	array_values(Array, Values), !.
fzn_to_prolog(range(L, U), FDRange) :-
	fdset_interval(FDRange, L, U), !.
fzn_to_prolog(IntSet, FDSet) :-
	set_values(IntSet, Integers),
	(   foreach(MX,Integers)
	do  integer(MX)
	), !,
	list_to_fdset(Integers, FDSet).
fzn_to_prolog(Set, Values) :-
	set_values(Set, Values), !.
fzn_to_prolog(Primitive, Primitive).


fzn_objective(State, Objective) :- 
	state_get([solve], State, [Conj]),
	fzn_get_objective(Conj, Objective), !.
fzn_objective(State, Objective) :-
	illarg(existence(objective, objective, ''),
	       fzn_objective(State, Objective), 1).

fzn_get_objective(solve(Options,_), Objective) :-
	member(minimize(Objective), Options), !.
fzn_get_objective(solve(Options,_), Objective) :-
	member(maximize(Objective), Options), !.
fzn_get_objective((Goal,_), Objective) :-
	fzn_get_objective(Goal, Objective).

%%%%% State %%%%%

state_get([], _, []).
state_get([W|WS], State, [T|TS]) :-
	state_get_one(W, State, T), !,
	state_get(WS, State, TS).

state_get_one(What, state(State), This) :- 
	memberchk(What-This, State).

state_set([], State, [], State).
state_set([W|WS], state(State0), [T|TS], State) :-
	selectchk(W-_, State0, State1), !,
	state_set(WS, state([W-T|State1]), TS, State).	

table_insert(Id, Table0, Value, Type, Table0) :-
	mutdict_get(Table0, Id, (Value0, Type0)), !,
	illarg(consistency((Value0, Type0), (Value, Type), ''),
	       table_insert(Id, Table0, Value, Type, _), 1).
table_insert(Id, Table0, Value, Type, Table0) :-
	mutdict_put(Table0, Id, (Value, Type)).

table_lookup(Id, Table, Value, Type) :-
	mutdict_get(Table, Id, (Value, Type)), !.
table_lookup(Id, Table, Value, Type) :-
	illarg(existence(identifier, Id, ''), table_lookup(Id, Table, Value, Type), 1).

table_lookup_value(Id, Table, Value) :-
	table_lookup(Id, Table, Value, _).

array_get(Index, Array, V) :-
	array_values(Array, Values),
	nth1(Index, Values, V).

%%%%% Interpret %%%%%

interpret_items(Stream, State0, State) :-
	(   read_item(Stream, Term)
	->  interpret(Term, State0, State1),
	    interpret_items(Stream, State1, State)
	;   interpret_deferred_items(State0, State)
	).


read_item(Stream, Term) :-
	stream_code(Stream, Code),
	clpfd:fzn_read_item(Code, Term, OK),
	(   Term = -1 -> fail
	;   OK = 1 -> true
	;   % illarg_syntax(''-[], read_item(Stream, Term), 1)
	    line_count(Stream, LC),
	    illarg(syntax(LC,'FlatZinc syntax error',[],0), read_item(Stream, Term), 1)
	).

interpret_deferred_items(State0, State) :-
	state_get([deferred,model], State0, [Deferred,Model3]),
	state_set([deferred,model], State0, [[],Model], State),
	(   foreach(defer(Elem),Deferred), % TODO: thread annotations to scalar_product
	    foreach(Tag-CB,KL1)
	do  classify_deferred(Elem, CB, Tag)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(Tag2-Clump,KL3),
	    fromto(Model,Model1,Model2,Model3)
	do  clump_to_bool_channel(Tag2, Clump, Model1, Model2)
	).

classify_deferred(table_bool_sicstus([R]-Ar,[L]-_), R, table(Ar,L)).
classify_deferred(table_int_sicstus([R]-Ar,[L]-_), R, table(Ar,L)).
classify_deferred(array_bool_element(X,[L]-_,Y), [X,Y], elem(L)).
classify_deferred(array_int_element(X,[L]-_,Y), [X,Y], elem(L)).
classify_deferred(int_eq_reif(X,Y,B), Y-B, channel([1],[X],#=)) :-
	var(X), !.
classify_deferred(int_eq_reif(Y,X,B), Y-B, channel([1],[X],#=)).
classify_deferred(int_ne_reif(X,Y,B), Y-B, channel([1],[X],#\=)) :-
	var(X), !.
classify_deferred(int_ne_reif(Y,X,B), Y-B, channel([1],[X],#\=)).
classify_deferred(int_le_reif(X,Y,B), Y-B, channel([1],[X],#=<)) :-
	var(X), !.
classify_deferred(int_le_reif(Y,X,B), Y-B, channel([1],[X],#>=)).
classify_deferred(int_lt_reif(X,Y,B), Y-B, channel([1],[X],#<)) :-
	var(X), !.
classify_deferred(int_lt_reif(Y,X,B), Y-B, channel([1],[X],#>)).
classify_deferred(int_lin_eq_reif([[-1]]-_,[Xs]-_,Y,B), NY-B, channel([1],Xs,#=)) :- !,
	NY is -Y.
classify_deferred(int_lin_ne_reif([[-1]]-_,[Xs]-_,Y,B), NY-B, channel([1],Xs,#\=)) :- !,
	NY is -Y.
classify_deferred(int_lin_lt_reif([[-1]]-_,[Xs]-_,Y,B), NY-B, channel([1],Xs,#>)) :- !,
	NY is -Y.
classify_deferred(int_lin_le_reif([[-1]]-_,[Xs]-_,Y,B), NY-B, channel([1],Xs,#>=)) :- !,
	NY is -Y.
classify_deferred(int_lin_eq_reif([As]-_,[Xs]-_,Y,B), Y-B, channel(As,Xs,#=)).
classify_deferred(int_lin_ne_reif([As]-_,[Xs]-_,Y,B), Y-B, channel(As,Xs,#\=)).
classify_deferred(int_lin_lt_reif([As]-_,[Xs]-_,Y,B), Y-B, channel(As,Xs,#<)).
classify_deferred(int_lin_le_reif([As]-_,[Xs]-_,Y,B), Y-B, channel(As,Xs,#=<)).

clump_to_bool_channel(table(Ar,T), Rows) --> [table(Rows,Ext)],
	{chop_array(Ar, T, Ext)}.
clump_to_bool_channel(elem(L), Tuples) -->
	(   foreach([X,Y],Tuples),
	    param(L)
	do  [element(X,L,Y)]
	).
clump_to_bool_channel(channel(As,Xs,Op), CBs1) -->
	{CBs1 = [_,_|_]},
	{keysort(CBs1, CBs2)},
	{CBs2 = [Min-_|_]},
	{Min1 is Min-1},
	{   foreach(Y2-B2,CBs2),
	    fromto(Min1-_,Y1-B1,Y2-B2,_),
	    fromto(Bs,Bs1,Bs2,[]),
	    fromto(Eqs,Eqs1,Eqs2,[])
	do  (   Y1=:=Y2 -> Bs1 = Bs2, Eqs1 = [B1 #= B2 | Eqs2] % merge variables
	    ;   Y1+1=:=Y2 -> Bs1 = [B2|Bs2], Eqs1 = Eqs2
	    )
	}, !,
	{bool_channel_goal(Bs, Var, Op, Min, BCGoal)},
	(foreach(Eq,Eqs) do [Eq]),
	(   {As = [1], Xs = [Var]}
	->  [BCGoal]
	;   {Bs = [B]}
	->  [scalar_product_reif(As,Xs,Op,Min,B)]
	;   [scalar_product(As,Xs,#=,Var), BCGoal]
	).
clump_to_bool_channel(channel([1],[Var],Op), CBs1) --> !,
	(   foreach(Y-B,CBs1),
	    param(Var,Op)
	do  [Cmp#<=>B],
	    {Cmp =.. [Op,Var,Y]}
	).
clump_to_bool_channel(channel(As,Xs,Op), CBs1) -->
	(   foreach(Y-B,CBs1),
	    param(As,Xs,Op)
	do  [scalar_product_reif(As,Xs,Op,Y,B)]
	).

bool_channel_goal([B], Var, Op, Min, (Cmp#<=>B)) :- !,
	Cmp =.. [Op,Var,Min].
bool_channel_goal(Bs, Var, Op, Min, bool_channel(Bs,Var,Op,Min)).

interpret(Predicate, State, State) :-
	Predicate = predicate(_), !. % predicates are currently ignored	
interpret(Decl, State0, State) :-
	Decl = decl(var, Type, Ident, Anns, []), !,
	subtype(Ident, Type, int),  % only integer variables are allowed
	var_init(Ident, Type, Anns, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Decl, State0, State) :-
	Decl = decl(var, Type, Ident, Anns, [Value0]), !,
	subtype(Ident, Type, int),   % only integer variables are allowed
	par_init(false, Type, Ident, Anns, Value0, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Decl, State0, State) :-
	Decl = decl(par, Type, Ident, Anns, Value0), !,
	par_init(true, Type, Ident, Anns, Value0, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Decl, State0, State) :-
	Decl = decl(vararray(1, Max), ElemType, Ident, Anns, []), !,
	subtype(Ident, array(Max, ElemType), array(Max, int)), % only integer variables are allowed
	vars_init(Max, ElemType, Ident, Anns, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Decl, State0, State) :-
	Decl = decl(vararray(1, Max), ElemType, Ident, Anns, [Array0]), !,
	subtype(Ident, array(Max, ElemType), array(Max, int)), % only integer variables are allowed
	pars_init(false, Max, ElemType, Ident, Anns, Array0, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Decl, State0, State) :-
	Decl = decl(pararray(1, Max), ElemType, Ident, Anns, [Array0]), !,
	pars_init(true, Max, ElemType, Ident, Anns, Array0, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Constraint, State0, State) :-
	Constraint = constraint(Elem, Anns), !,
	interpret_constraint(Elem, Anns, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Solve, State0, State) :-
	Solve = solve(Anns, Kind), !,
	interpret_solve(Kind, Anns, State0, RestAnns, State),
	anns_not_supported(RestAnns).
interpret(Item, State0, State) :-
	illarg(system('FlatZinc interpreter error, please report this'),
	       interpret(Item, State0, State), 1).

%%%%% Declarations %%%%%

% Par is true or false
par_init(Par, Type, Ident, Anns, Value0, State0, RestAnns, State) :-
	state_get([table, vars, output_vars], State0,
		  [Table0, Vars0, OutputVars0]),
	type(Value0, Table0, Type),
	table_insert(Ident, Table0, Value0, Type, Table),
	(   Par == true
	->  Vars       = Vars0,
	    RestAnns   = Anns,
	    OutputVars = OutputVars0
	;   (   selectchk(is_defined_var, Anns, Anns1)
	    ->  Vars = Vars0
	    ;   deref(Value0, Table0, Value),
		Vars = [Value|Vars0],
		Anns1 = Anns
	    ),
	    (   selectchk(output_var, Anns1, RestAnns)
	    ->  ord_add_element(OutputVars0, Ident-[], OutputVars)
	    ;   OutputVars = OutputVars0,
		RestAnns = Anns1
	    )
	),
	state_set([table, vars, output_vars], State0,
		  [Table, Vars, OutputVars], State).

% Par is true or false
pars_init(Par, N, ElemType, Ident, Anns, Array, State0, RestAnns, State) :-
	state_get([table, vars, output_vars], State0,
		  [Table0, Vars0, OutputVars0]),
	type(Array, Table0, array(N, ElemType)),
	Array = VsAvl,
	table_insert(Ident, Table0, VsAvl, array(N, ElemType), Table),
	(   Par == true
	->  Vars = Vars0,
	    RestAnns = Anns,
	    OutputVars = OutputVars0
	;   (   selectchk(is_defined_var, Anns, Anns1)
	    ->  Vars = Vars0
	    ;   deref(Array, Table0, Array1),
	    	array_values(Array1, Values1),
	    	append(Values1, Vars0, Vars),
	    	Anns1 = Anns
	    ),
	    (   selectchk(output_array(IndexRanges0), Anns1, RestAnns)
	    ->  array_values(IndexRanges0, IndexRanges),
		ord_add_element(OutputVars0, Ident-IndexRanges, OutputVars)
	    ;   OutputVars = OutputVars0,
		RestAnns = Anns1
	    )
	),
	state_set([table, vars, output_vars], State0,
		  [Table, Vars, OutputVars], State).

var_init(Ident, Type, Anns, State0, RestAnns, State) :-
	type_to_domain(Type, Domain),
	state_get([table, vars, output_vars, model], State0,
		  [Table0, Vars0, OutputVars0, Model0]),
	X in_set Domain,	% double posting due to type checking
	Model = [X in_set Domain | Model0],
	table_insert(Ident, Table0, X, Type, Table),
	(   selectchk(is_defined_var, Anns, Anns1)
	->  Vars = Vars0
	;   Vars = [X|Vars0],
	    Anns1 = Anns
	),
	(   selectchk(output_var, Anns1, RestAnns)
	->  ord_add_element(OutputVars0, Ident-[], OutputVars)
	;   OutputVars = OutputVars0,
	    RestAnns = Anns1
	),
	state_set([table, vars, output_vars, model], State0,
		  [Table, Vars, OutputVars, Model], State).

vars_init(N, ElemType, Ident, Anns, State0, RestAnns, State) :-
	state_get([table, vars, output_vars, model], State0,
		  [Table0, Vars0, OutputVars0, Model0]),
	type_to_domain(ElemType, Domain),
	vars_domain_init(N, Domain, Vars1, Model, Model0),
	array_values(VarsAvl, Vars1),
	table_insert(Ident, Table0, VarsAvl, array(N, ElemType), Table),
	(   selectchk(is_defined_var, Anns, Anns1)
	->  Vars = Vars0
	;   append(Vars1, Vars0, Vars),
	    Anns1 = Anns
	),
	(   selectchk(output_array(IndexRanges0), Anns1, RestAnns)
	->  array_values(IndexRanges0, IndexRanges),
	    ord_add_element(OutputVars0, Ident-IndexRanges, OutputVars)
	;   OutputVars = OutputVars0,
	    RestAnns = Anns1
	),
	state_set([table, vars, output_vars, model], State0,
		  [Table, Vars, OutputVars, Model], State).

vars_domain_init(N, Domain, Vars) --> [domain(Vars, L, U)],
	{fdset_interval(Domain, L, U)}, !,
	{length(Vars, N)},
	{domain(Vars, L, U)}.   % double posting due to type checking	
vars_domain_init(N, Domain, Vars) -->
	vars_domain_init(0, N, Domain, Vars).

vars_domain_init(M, M, _, []) --> !.
vars_domain_init(M, N, S, [X|Xs]) --> [X in_set S],
	{X in_set S},		% double posting due to type checking
	{M1 is M + 1},
	vars_domain_init(M1, N, S, Xs).

%%%%% Constraints %%%%%

interpret_constraint(Elem, Anns0, State0, Anns, State) :-
	state_get([table, model, deferred], State0, [Table0, Model0, Deferred0]),
	Elem =.. [Name|Args0],
	deref_list(Args0, Table0, Args1),
	add_array_lengths(Args1, Args),
	(   fzn_constraint_clpfd(Name, Args, Anns0, Goals, Types, Anns)
	->  (   foreach(A0, Args0),
		foreach(T, Types),
		param(Table0)
	    do  type(A0, Table0, T)
	    ),
	    (   fzn_defer(Elem, Args) ->
		DeferElem =.. [Name|Args],
		state_set([deferred], State0, [[defer(DeferElem/*,Anns0*/)|Deferred0]], State)
	    ;   append(Goals, Model0, Model),
		state_set([model], State0, [Model], State)
	    )
	;   length(Args, Arity),
	    illarg(existence(constraint, Name/Arity, ''),
		   interpret_constraint(Elem, Anns0, State0, Anns, State), 1)
	).

:- fzn_defer/2 is nondet.
fzn_defer(int_eq_reif(_,_,_), [X,Y,_]) :- \+bool_cmp(X, Y), \+var_cmp(X, Y).
fzn_defer(int_ne_reif(_,_,_), [X,Y,_]) :- \+bool_cmp(X, Y), \+var_cmp(X, Y).
fzn_defer(int_lt_reif(_,_,_), [X,Y,_]) :- \+bool_cmp(X, Y), \+var_cmp(X, Y).
fzn_defer(int_le_reif(_,_,_), [X,Y,_]) :- \+bool_cmp(X, Y), \+var_cmp(X, Y).
fzn_defer(int_lin_eq_reif(_,_,Y,_), _) :- integer(Y).
fzn_defer(int_lin_ne_reif(_,_,Y,_), _) :- integer(Y).
fzn_defer(int_lin_lt_reif(_,_,Y,_), _) :- integer(Y).
fzn_defer(int_lin_le_reif(_,_,Y,_), _) :- integer(Y).
fzn_defer(array_bool_element(_,_,_), _). % since 4.3.1
fzn_defer(array_int_element(_,_,_), _).  % since 4.3.1
fzn_defer(table_bool_sicstus(_,_), _).   % since 4.3.1
fzn_defer(table_int_sicstus(_,_), _).    % since 4.3.1

bool_cmp(X, Y) :-
	fd_min(X, Xmin), Xmin >= 0,
	fd_max(X, Xmax), Xmax =< 1,
	fd_min(Y, Ymin), Ymin >= 0,
	fd_max(Y, Ymax), Ymax =< 1.

var_cmp(X, Y) :- var(X), var(Y).

add_array_lengths([A|Args0], [A|Args]) :-
	var(A), !,
	add_array_lengths(Args0, Args).
add_array_lengths([], []).
add_array_lengths([A0|Args0], [A|Args]) :-
	(   array_values(A0, Values)
	->  length(Values, L),
	    A = A0-L
	;   A = A0
	),
	add_array_lengths(Args0, Args).

fzn_constraint_clpfd(int_eq, [X, Y], Anns, [X #= Y], [int, int], Anns).
fzn_constraint_clpfd(int_ne, [X, Y], Anns, [X #\= Y], [int, int], Anns).
fzn_constraint_clpfd(int_lt, [X, Y], Anns, [X #< Y], [int, int], Anns).
fzn_constraint_clpfd(int_le, [X, Y], Anns, [X #=< Y], [int, int], Anns).

fzn_constraint_clpfd(int_eq_reif,  [X, Y, B], Anns,
		     [Goal], [int, int, bool], Anns) :-
	bool_cmp_goal(X, Y, B, 1, Goal), !.
fzn_constraint_clpfd(int_ne_reif,  [X, Y, B], Anns,
		     [Goal], [int, int, bool], Anns) :-
	bool_cmp_goal(X, Y, B, 2, Goal), !.
fzn_constraint_clpfd(int_lt_reif,  [X, Y, B], Anns,
		     [Goal], [int, int, bool], Anns) :-
	bool_cmp_goal(X, Y, B, 3, Goal), !.
fzn_constraint_clpfd(int_le_reif,  [X, Y, B], Anns,
		     [Goal], [int, int, bool], Anns) :-
	bool_cmp_goal(X, Y, B, 4, Goal), !.
fzn_constraint_clpfd(int_eq_reif,  [X, Y, B], Anns,
		     [(X #= Y) #<=> B], [int, int, bool], Anns).
fzn_constraint_clpfd(int_ne_reif,  [X, Y, B], Anns,
		     [(X #\= Y) #<=> B], [int, int, bool], Anns).
fzn_constraint_clpfd(int_lt_reif,  [X, Y, B], Anns,
		     [(X #< Y) #<=> B], [int, int, bool], Anns).
fzn_constraint_clpfd(int_le_reif,  [X, Y, B], Anns,
		     [(X #=< Y) #<=> B], [int, int, bool], Anns).

fzn_constraint_clpfd(bool_eq,  [X, Y], Anns, [X #= Y], [int, int], Anns).
fzn_constraint_clpfd(bool_lt,  [X, Y], Anns, [X=0, Y=1], [int, int], Anns).
fzn_constraint_clpfd(bool_le,  [X, Y], Anns, [X #=> Y], [int, int], Anns).

fzn_constraint_clpfd(bool_eq_reif, [X, Y, B], Anns,
		     [(X #<=> Y) #<=> B], [int, int, bool], Anns).
fzn_constraint_clpfd(bool_lt_reif, [X, Y, B], Anns,
		     [(#\X #/\ Y) #<=> B], [int, int, bool], Anns).
fzn_constraint_clpfd(bool_le_reif, [X, Y, B], Anns,
		     [(X #=> Y) #<=> B], [int, int, bool], Anns).

fzn_constraint_clpfd(int_lin_eq, [[A]-M, [X]-N, C], Anns0,
		     [Constraint],
		     [array(M, int), array(N, int), int], Anns) :-
	delete_zero_terms(A, X, A1, X1),
	strength_reduce(scalar_product(A1, X1, #=, C, Options), Constraint),
	anns_consistency_option(Anns0, Anns, Options).
fzn_constraint_clpfd(int_lin_ne, [[A]-M, [X]-N, C], Anns,
		     [scalar_product(A1, X1, #\=,C)],
		     [array(M, int), array(N, int), int], Anns) :-
	delete_zero_terms(A, X, A1, X1).
fzn_constraint_clpfd(int_lin_le, [[A]-M, [X]-N, C], Anns,
		     [scalar_product(A1, X1, #=<,C)],
		     [array(M, int), array(N, int), int], Anns) :-
	delete_zero_terms(A, X, A1, X1).

fzn_constraint_clpfd(int_lin_eq_reif, [[A]-M, [X]-N, C, B], Anns0,
		     Exp,
		     [array(M, int), array(N, int), int, bool], Anns) :-
	(   A = [ 1] -> X = [X0], Exp = [X0 #=  C #<=> B]
	;   A = [-1] -> X = [X0], Exp = [X0 #= NC #<=> B], NC is -C
	;   true     ->
	    delete_zero_terms(A, X, A1, X1),
	    Exp = [scalar_product_reif(A1, X1, #=, C, B, Options)]
	),
	anns_consistency_option(Anns0, Anns, Options).
fzn_constraint_clpfd(int_lin_ne_reif, [[A]-M, [X]-N, C, B], Anns,
		     Exp,
		     [array(M, int), array(N, int), int, bool], Anns) :-
	(   A = [ 1] -> X = [X0], Exp = [X0 #\=  C #<=> B]
	;   A = [-1] -> X = [X0], Exp = [X0 #\= NC #<=> B], NC is -C
	;   true     ->
	    delete_zero_terms(A, X, A1, X1),
	    Exp = [scalar_product_reif(A1, X1, #\=, C, B)]
	).
fzn_constraint_clpfd(int_lin_le_reif, [[A]-M, [X]-N, C, B], Anns,
		     Exp,
		     [array(M, int), array(N, int), int, bool], Anns) :- 
	(   A = [ 1] -> X = [X0], Exp = [X0 #=<  C #<=> B]
	;   A = [-1] -> X = [X0], Exp = [X0 #>= NC #<=> B], NC is -C
	;   true     ->
	    delete_zero_terms(A, X, A1, X1),
	    Exp = [scalar_product_reif(A1, X1, #=<, C, B)]
	).

fzn_constraint_clpfd(int_plus, [X, Y, Z], Anns0,
		     Goals, [int, int, int], Anns) :-
	anns_consistency_option(Anns0, Anns, Options),
	(   Options = [consistency(domain)]
	->  Goals = [scalar_product([1, 1], [X, Y], #=, Z, Options)]
	;   Goals = [X+Y #= Z]
	).
fzn_constraint_clpfd(int_times,  [X, Y, Z], Anns,
		     [X*Y #= Z], [int, int, int], Anns).
fzn_constraint_clpfd(int_div,    [X, Y, Z], Anns,
		     [X/Y #= Z], [int, int, int], Anns).
/* [MA] note that FlatZinc mod is SICStus rem! */
fzn_constraint_clpfd(int_mod,    [X, Y, Z], Anns,
		     [X rem Y #= Z], [int, int, int], Anns).
fzn_constraint_clpfd(int_min,    [X, Y, Z], Anns,
		     [min(X, Y) #= Z], [int, int, int], Anns).
fzn_constraint_clpfd(int_max,    [X, Y, Z], Anns,
		     [max(X, Y) #= Z], [int, int, int], Anns).
fzn_constraint_clpfd(int_abs,    [X, Z],    Anns,
		     [abs(X) #= Z], [int, int], Anns).

fzn_constraint_clpfd(bool_and, [X, Y, B], Anns,
		     [(X #/\ Y) #<=> B], [bool, bool, bool], Anns).
fzn_constraint_clpfd(bool_or, [X, Y, B], Anns,
		     [(X #\/ Y) #<=> B], [bool, bool, bool], Anns).
fzn_constraint_clpfd(bool_equiv, [X, Y, B], Anns,
		     [(X #<=> Y) #<=> B], [bool, bool, bool], Anns).
fzn_constraint_clpfd(bool_xor, [X, Y, B], Anns,
		     [(X #\ Y) #<=> B], [bool, bool, bool], Anns).
fzn_constraint_clpfd(bool_not, [X, B], Anns,
		     [(#\ X) #<=> B], [bool, bool], Anns).

fzn_constraint_clpfd(array_bool_and, [[A]-M, B], Anns,
		     [U #= B]/* 4.4 */, [array(M,bool), bool], Anns) :-
	A = [U], !.
fzn_constraint_clpfd(array_bool_and, [[A]-M, B], Anns,
		     [bool_and(A,B)]/* 4.3 */, [array(M,bool), bool], Anns).
fzn_constraint_clpfd(array_bool_or, [[A]-M, B], Anns,
		     [U #= B]/* 4.4 */, [array(M,bool), bool], Anns) :-
	A = [U], !.
fzn_constraint_clpfd(array_bool_or, [[A]-M, B], Anns,
		     [bool_or(A,B)]/* 4.3 */, [array(M,bool), bool], Anns).
fzn_constraint_clpfd(array_bool_xor, [[A]-M], Anns,
		     [U = 1]/* 4.4 */, [array(M, bool)], Anns) :-
	A = [U], !.
fzn_constraint_clpfd(array_bool_xor, [[A]-M], Anns,
		     [bool_xor(A,1)]/* 4.3 */, [array(M, bool)], Anns).
fzn_constraint_clpfd(bool_clause, [[A]-M, [B]-N], Anns,
		     [bool_or(AC,1)]/* 4.3 */,
		     [array(M, bool), array(N, bool)], Anns) :-
	append(A, C, AC),
	(   foreach(X,B),
	    foreach(#\ X,C)
	do  true
	).

% [MC] new for minizinc-1.5
fzn_constraint_clpfd(bool_lin_le, [[A]-M, [X]-N, C], Anns,
		     [scalar_product(A,X, #=<,C)],
		     [array(M, int), array(N, bool), int], Anns).

% [MC] new for minizinc-1.5
fzn_constraint_clpfd(bool_lin_eq, [[A]-M, [X]-N, C], Anns,
		     [Constraint],
		     [array(M, int), array(N, bool), int], Anns) :-
	delete_zero_terms(A, X, A1, X1),
	strength_reduce(scalar_product(A1, X1, #=, C, []), Constraint).

fzn_constraint_clpfd(set_in, [X, S0], Anns,
		     [X in_set S], [int, set(int)], Anns) :-
	type_to_fdset(S0, S).

fzn_constraint_clpfd(set_in_reif, [X, S0, B], Anns,
		     [(X in_set S) #<=> B], [int, set(int), bool], Anns) :-
	type_to_fdset(S0, S).

fzn_constraint_clpfd(set_card, [S0, X], Anns, [X #= C], [set(int), int], Anns) :-
	type_to_fdset(S0, S),
	fdset_size(S, C).

fzn_constraint_clpfd(array_bool_element, [X, [A]-M, Y], Anns,
		     [Ax #= Y], [int, array(M, bool), bool], Anns) :-
	integer(X), !,
	nth1(X, A, Ax).
fzn_constraint_clpfd(array_bool_element, [X, [A]-M, Y], Anns,
		     [element(X, A, Y)], [int, array(M, bool), bool], Anns).
fzn_constraint_clpfd(array_var_bool_element, [X, [A]-M, Y], Anns,
		     [Ax #= Y], [int, array(M, bool), bool], Anns) :-
	integer(X), !,
	nth1(X, A, Ax).
fzn_constraint_clpfd(array_var_bool_element, [X, [A]-M, Y], Anns,
		     [element(X, A, Y)], [int, array(M, bool), bool], Anns).
fzn_constraint_clpfd(array_int_element, [X, [A]-M, Y], Anns,
		     [Ax #= Y], [int, array(M, int), int], Anns) :-
	integer(X), !,
	nth1(X, A, Ax).
fzn_constraint_clpfd(array_int_element, [X, [A]-M, Y], Anns,
		     [element(X, A, Y)], [int, array(M, int), int], Anns).
fzn_constraint_clpfd(array_var_int_element, [X, [A]-M, Y], Anns,
		     [Ax #= Y], [int, array(M, int), int], Anns) :-
	integer(X), !,
	nth1(X, A, Ax).
fzn_constraint_clpfd(array_var_int_element, [X, [A]-M, Y], Anns,
		     [element(X, A, Y)], [int, array(M, int), int], Anns).

fzn_constraint_clpfd(bool2int, [B, X], Anns, [B #= X], [bool, int], Anns).

% all_different_int(array[int] of var int: x)
% [MC] 4.3: for a while, default reverted to bounds-consistency
fzn_constraint_clpfd(fzn_all_different_int, [[A]-M], Anns0,
		     [all_distinct(A, Options)], [array(M, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options).

% all_equal_int(array[int] of var int: x)
fzn_constraint_clpfd(fzn_all_equal_int, [[A]-M], Anns, [A=B], [array(M, int)], Anns) :-
	(   foreach(_,A),
	    foreach(Y,B),
	    param(Y)
	do  true
	).

% all_equal_int_reif(array[int] of var int: x, var bool: b)
fzn_constraint_clpfd(fzn_all_equal_int_reif, [[A]-M, B], Anns,
		     [all_equal_reif(A,B)], [array(M, int), bool], Anns).

% cumulative(array[int] of var int: s, array[int] of var int: d,
%            array[int] of var int: r, var int: b)
% [MC] 4.3: cumulative/2 does not prune heights
fzn_constraint_clpfd(fzn_cumulative, [[S]-M, [D]-N, [R]-O, B], Anns0,
		     [cumulative(Tasks, [limit(B)|Options])],
		     [array(M, int), array(N, int), array(O, int), int],
		     Anns) :-
	(   select(domain, Anns0, Anns)
	->  Options = [global(true)]
	;   Anns0 = Anns,
	    Options = []
	),
	(   foreach(Si,S),
	    foreach(Di,D),
	    foreach(Ri,R),
	    foreach(task(Si,Di,_,Ri,I),Tasks),
	    count(I,1,_)
	do  true
	).

% decreasing_bool(array[int] of var bool: x)
% increasing_bool(array[bool] of var bool: x)
fzn_constraint_clpfd(pairwise_relation_bool_sicstus, [[X]-M, RelOpSpec], Anns,
		     Goals, [array(M, int), int], Anns) :-
	pairwise_relation(X, RelOpSpec, Goals).

% decreasing_int(array[int] of var int: x)
% increasing_int(array[int] of var int: x)
fzn_constraint_clpfd(pairwise_relation_int_sicstus, [[X]-M, RelOpSpec], Anns,
		     Goals, [array(M, int), int], Anns) :-
	pairwise_relation(X, RelOpSpec, Goals).

% predicate diffn(array[int] of var int: x,
%                 array[int] of var int: y,
%                 array[int] of var int: dx,
%                 array[int] of var int: dy)
fzn_constraint_clpfd(fzn_diffn, [[Xs]-XN, [Ys]-YN, [DXs]-DXN, [DYs]-DYN], Anns0,
		     Goals,
		     [array(XN, int), array(YN, int), array(DXN, int), array(DYN, int)],
		     Anns) :-
	(   foreach(X,Xs),
	    foreach(DX,DXs),
	    foreach(Y,Ys),
	    foreach(DY,DYs),
	    foreach([X-DX,Y-DY],Items)
	do  true
	),
	anns_global_option(Anns0, Anns, Options),
	diffn_implied(Xs, Ys, DXs, DYs, Options, Goals, [diffn(Items, [strict(true)])]).

% predicate diffn_nonstrict(array[int] of var int: x,
%                           array[int] of var int: y,
% 			  array[int] of var int: dx,
% 			  array[int] of var int: dy)
fzn_constraint_clpfd(fzn_diffn_nonstrict, [[Xs]-XN, [Ys]-YN, [DXs]-DXN, [DYs]-DYN], Anns0,
		     Goals,
		     [array(XN, int), array(YN, int), array(DXN, int), array(DYN, int)],
		     Anns) :-
	(   foreach(X,Xs),
	    foreach(DX,DXs),
	    foreach(Y,Ys),
	    foreach(DY,DYs),
	    foreach([X-DX,Y-DY],Items)
	do  true
	),
	anns_global_option(Anns0, Anns, Options),
	diffn_implied(Xs, Ys, DXs, DYs, Options, Goals, [diffn(Items, [strict(false)])]).

% global_cardinality(array[int] of var int: x,
%		     array[int] of int: cover,
%                    array[int] of var int: counts)
% global_cardinality_closed(array[int] of var int: x,
%                           array[int] of int: cover,
%                           array[int] of var int: counts)
fzn_constraint_clpfd(fzn_global_cardinality,
		     [[X]-M, [Cover]-N, [Counts]-O], Anns0,
		     [global_cardinality(X, KeyCounts, Options)],
		     [array(M, int), array(N, int), array(O, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options), % [MC] 4.4: defaulting to BC version ran slower
	(   foreach(Key, Cover),
	    foreach(Count, Counts),
	    foreach(Key-Count, KeyCounts0)
	do  true
	),
	gcc_add_unconstrained(Cover, X, UnconstrainedKeyCounts),
	append(UnconstrainedKeyCounts, KeyCounts0, KeyCounts).
fzn_constraint_clpfd(fzn_global_cardinality_closed,
		     [[X]-M, [Cover]-N, [Counts]-O], Anns0,
		     [global_cardinality(X, KeyCounts, Options)],
		     [array(M, int), array(N, int), array(O, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options), % [MC] 4.4: defaulting to BC version ran slower
	(   foreach(Key, Cover),
	    foreach(Count, Counts),
	    foreach(Key-Count, KeyCounts)
	do  true
	).


% global_cardinality_low_up(array[int] of var int: x,
%                           array[int] of int: cover,
%                           array[int] of int: lbound,
%                           array[int] of int: ubound)
% global_cardinality_low_up_closed(array[int] of var int: x,
%                                  array[int] of int: cover,
%                                  array[int] of int: lbound,
%                                  array[int] of int: ubound)
fzn_constraint_clpfd(fzn_global_cardinality_low_up,
		     [[X]-M, [Cover]-N, [L]-O, [U]-P], Anns0,
		     Expansion,
		     [array(M, int), array(N, int), array(O, int), array(P, int)],
		     Anns) :-
	anns_consistency_option(Anns0, Anns, Options), % [MC] 4.4: defaulting to BC version ran slower
	(   foreach(Key, Cover),
	    foreach(Low, L),
	    foreach(Up, U),
	    foreach(Key-Count, KeyCounts0),
	    foreach(Count in Low..Up, Decl)
	do  Count in Low..Up
	),
	append(Decl, [global_cardinality(X, KeyCounts, Options)], Expansion),
	gcc_add_unconstrained(Cover, X, UnconstrainedKeyCounts),
	append(UnconstrainedKeyCounts, KeyCounts0, KeyCounts).
fzn_constraint_clpfd(fzn_global_cardinality_low_up_closed,
		     [[X]-M, [Cover]-N, [L]-O, [U]-P], Anns0,
		     Expansion,
		     [array(M, int), array(N, int), array(O, int), array(P, int)],
		     Anns) :-
	anns_consistency_option(Anns0, Anns, Options), % [MC] 4.4: defaulting to BC version ran slower
	(   foreach(Key, Cover),
	    foreach(Low, L),
	    foreach(Up, U),
	    foreach(Key-Count, KeyCounts),
	    foreach(Count in Low..Up, Decl)
	do  Count in Low..Up
	),
	append(Decl, [global_cardinality(X, KeyCounts, Options)], Expansion).

% inverse(array[int] of var int: f, array[int] of var int: invf)
fzn_constraint_clpfd(inverse_sicstus,
		     [[F]-M, [InvF]-N, MinIndexF, MinIndexInvF],
		     Anns0,
		     G, [array(M, int), array(N, int), int, int], Anns) :-
	anns_consistency_option(Anns0, Anns, Options),
	(   MinIndexF = 1, MinIndexInvF = 1
	->  G = [assignment(F, InvF, Options)]
	;   (   foreach(X, F),
		foreach(X1, F1),
		foreach(O1, Offsets1),
		param(MinIndexInvF)
	    do  O1 = (X1 #= X - MinIndexInvF + 1)
	    ),
	    (   foreach(Y, InvF),
		foreach(Y1, InvF1),
		foreach(O2, Offsets2),
		param(MinIndexF)	      
	    do  O2 = (Y1 #= Y - MinIndexF + 1)
	    ),
	    append([Offsets1, Offsets2, [assignment(F1, InvF1, Options)]], G)
	).

% lex_less_int(array[int] of var int: x, array[int] of var int: y)
% lex_lesseq_int(array[int] of var int: x, array[int] of var int: y)
% lex_less_bool(array[int] of var bool: x, array[int] of var bool: y)
% lex_lesseq_bool(array[int] of var bool: x, array[int] of var bool: y)
fzn_constraint_clpfd(fzn_lex_less_int, [[X]-M, [Y]-N], Anns,
		     [lex_chain([X, Y], [op(#<)])],
		     [array(M, int), array(N, int)], Anns).
fzn_constraint_clpfd(fzn_lex_lesseq_int, [[X]-M, [Y]-N], Anns,
		     [lex_chain([X, Y], [op(#=<)])],
		     [array(M, int), array(N, int)], Anns).
fzn_constraint_clpfd(fzn_lex_less_bool, [[X]-M, [Y]-N], Anns,
		     [lex_chain([X, Y], [op(#<)])],
		     [array(M, bool), array(N, bool)], Anns).
fzn_constraint_clpfd(fzn_lex_lesseq_bool, [[X]-M, [Y]-N], Anns,
		     [lex_chain([X, Y], [op(#=<)])],
		     [array(M, bool), array(N, bool)], Anns).

% lex2(array[int, int] of var int: x)
% strict_lex2(array[int, int] of var int: x)
fzn_constraint_clpfd(lex2_int_sicstus, [[X]-M, NRows, NColumns, RelOpSpec], Anns,
		     [lex_chain(Rows, [op(RelOp)]),
		      lex_chain(Columns, [op(RelOp)])],
		     [array(M, int), int, int, int], Anns) :-
	relop_from_spec(RelOpSpec, RelOp),
	(   for(_, 1, NRows),
	    fromto(X, X1, X2, []),
	    foreach(Row, Rows),
	    param(NColumns)
	do  append_length(Row, X2, X1, NColumns)
	),
	transpose(Rows, Columns).

% member_bool(array[int] of var bool: x, var bool: y)
fzn_constraint_clpfd(fzn_member_bool, [[X]-M, Y], Anns,
		     [element(X, Y)], [array(M, int), int], Anns).

% member_int(array[int] of var int: x, var int: y)
fzn_constraint_clpfd(fzn_member_int, [[X]-M, Y], Anns,
		     [element(X, Y)], [array(M, int), int], Anns).

% nvalue(var int: n, array[int] of var int: x)
fzn_constraint_clpfd(fzn_nvalue, [C, [X]-N], Anns,
		     [nvalue(C, X)], [int, array(N, int)], Anns).

% sort(array[int] of var int: x, array[int] of var int: y)
fzn_constraint_clpfd(fzn_sort, [[X]-M, [Y]-N], Anns,
		     [domain(Ps, 1, N), sorting(X, Ps, Y)],
		     [array(M, int), array(N, int)], Anns) :-
	length(Ps, N).


% sum_pred(var int: i,
%          array[int] of set of int: sets,
%          array[int] of int: cs,
%          var int: s)
fzn_constraint_clpfd(sum_pred_sicstus,
		     [Index, [Sets]-M, [Cs]-N, Sum, MinIndex], Anns,
		     G,
		     [int, array(M, set(int)), array(N, int), int, int],
		     Anns) :-
	(   foreach(Set, Sets),
	    foreach(SetSum, SetsSums),
	    param(Cs, MinIndex)
	do  set_values(Set, Indices),
	    (   foreach(I, Indices),
		fromto(0, S0, S1, SetSum),
		param(Cs, MinIndex)
	    do  I1 is I - MinIndex,
		nth0(I1, Cs, C),
		S1 is S0 + C
	    )
	),
	fzn_element_clpfd(MinIndex, Index, SetsSums, Sum, G).

% table_bool(array[int] of var bool: x, array[int] of bool: t)
fzn_constraint_clpfd(table_bool_sicstus, [[X]-M, [T]-N], Anns0,
		     [table([X], Ext, Options)],
		     [array(M, int), array(N, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options),
	chop_array(M, T, Ext).

% table_int(array[int] of var int: x, array[int] of int: t)
fzn_constraint_clpfd(table_int_sicstus, [[X]-M, [T]-N], Anns0,
		     [table([X], Ext, Options)],
		     [array(M, int), array(N, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options),
	chop_array(M, T, Ext).

% [MC] new for minizinc-1.4

% predicate bin_packing(int: c,
%                       array[int] of var int: bin,
%                       array[int] of int: w)
fzn_constraint_clpfd(fzn_bin_packing, [Capa, [Bin]-N, [Weight]-O], Anns,
		     G, [int, array(N, int), array(O, int)], Anns) :-
	fzn_bin_packing_clpfd(Capa, Bin, Weight, G, []).

% bin_packing_capa(array[int] of int: capa,
%                  array[int] of var int: bin,
% 		   array[int] of int: weight)
fzn_constraint_clpfd(bin_packing_capa_sicstus, [[Capa]-M, [Bin]-N, [Weight]-O, MinIndex], Anns,
		     G, [array(M, int), array(N, int), array(O, int), int], Anns) :-
	fzn_bin_packing_capa_clpfd(Capa, Bin, Weight, MinIndex, G, []).

% bin_packing_load(array[int] of var int: load,
%                  array[int] of var int: bin,
% 		   array[int] of int: weight)
fzn_constraint_clpfd(bin_packing_load_sicstus, [[Load]-M, [Bin]-N, [Weight]-O, MinIndex], Anns,
		     G, [array(M, int), array(N, int), array(O, int), int], Anns) :-
	fzn_bin_packing_load_clpfd(Load, Bin, Weight, MinIndex, G, []).

% predicate distribute(array[int] of var int: card,
%                      array[int] of var int: value,
%                      array[int] of var int: base)
fzn_constraint_clpfd(fzn_distribute, [[Card]-M, [Value]-N, [Base]-O], Anns0, G,
		     [array(M, int), array(N, int), array(O, int)], Anns) :-
	anns_consistency_option(Anns0, Anns, Options),
	fzn_distribute_clpfd(Card, Value, Base, Options, G, []).	

% predicate value_precede_int(int: s, int: t, array[int] of var int: x)
fzn_constraint_clpfd(fzn_value_precede_int, [S, T, [X]-M], Anns, G,
		     [int, int, array(M, int)], Anns) :-
	fzn_value_precede_clpfd(S, T, X, G, []).

% predicate value_precede_chain_int(array[int] of int: c, array[int] of var int: x)
fzn_constraint_clpfd(fzn_value_precede_chain_int, [[C]-M, [X]-N], Anns, G,
		     [array(M, int), array(N, int)], Anns) :-
	fzn_value_precede_chain_clpfd(C, X, G, []).

% predicate seq_precede_chain_int(array[int] of var int: x)
fzn_constraint_clpfd(fzn_seq_precede_chain_int, [[X]-N], Anns, G,
		     [array(N, int)], Anns) :-
	fzn_seq_precede_chain_clpfd(X, G, []).

% predicate regular(array[int] of var int: x, int: Q, int: S,
%                   array[int,int] of int: d, int: q0, set of int: F) =
fzn_constraint_clpfd(regular_sicstus, [[X]-M, Q, S, [D]-N, Q0, F],
		     Anns, G,
		     [array(M,int), int, int, array(N,int), int, set(int)], Anns) :-
	fzn_regular_clpfd(X, Q, S, D, Q0, F, G, []).

% predicate cost_regular(array[int] of var int: x, int: Q, int: S,
%                   array[int,int] of int: d, int: q0, set of int: F,
%                   array[int,int] of int: c, var int: C);
fzn_constraint_clpfd(cost_regular_sicstus, [[X]-M, Q, S, [DD]-N, Q0, F, [CC]-N, C],
		     Anns, G,
		     [array(M,int), int, int, array(N,int), int, set(int), array(N,int), int], Anns) :-
	fzn_cost_regular_clpfd(X, Q, S, DD, Q0, F, CC, C, G, []).

% predicate fzn_mdd(array[int] of var int: x,
%                   int: N,
% 		  array[int] of int: level,
% 		  int: E,
% 		  array[int] of int: from,
% 		  array[int] of set of int: label,
% 		  array[int] of int: to);
fzn_constraint_clpfd(mdd_sicstus, [[X]-NV, [Level]-NN, [From]-NE, [Label]-NE, [To]-NE],
		     Anns, G,
		     [array(NV,int), array(NN,int), array(NE,int), array(NE,set(int)), array(NE,int)], Anns) :-
	fzn_mdd_clpfd(X, Level, From, Label, To, G, []).

% predicate fzn_cost_mdd(array[int] of var int: x,
%                   int: N,
% 		  array[int] of int: level,
% 		  int: E,
% 		  array[int] of int: from,
% 		  array[int] of set of int: label,
% 		  array[int] of int: to);
fzn_constraint_clpfd(cost_mdd_sicstus, [[X]-NV, [Level]-NN, [From]-NE, [Label]-NE, [To]-NE, [Cost]-NE, TotalCost],
		     Anns, G,
		     [array(NV,int), array(NN,int), array(NE,int), array(NE,set(int)), array(NE,int), array(NE,int), int], Anns) :-
	fzn_cost_mdd_clpfd(X, Level, From, Label, To, Cost, TotalCost, G, []).

% predicate among(var int: n, array[int] of var int: x, set of int: v)
fzn_constraint_clpfd(fzn_among, [N, [X]-M, V], Anns, G, [int, array(M,int), set(int)], Anns) :-
	fzn_among_clpfd(N, X, V, G, []).

% [MC] new for minizinc-1.5

% predicate circuit(array[int] of var int)
fzn_constraint_clpfd(circuit_sicstus, [[S]-N,IX], Anns, G, [array(N, int), set(int)], Anns) :-
	fzn_circuit_clpfd(S, IX, G, []).

% predicate subcircuit(array[int] of var int)
fzn_constraint_clpfd(subcircuit_sicstus, [[S]-N,IX], Anns, G, [array(N, int), set(int)], Anns) :-
	fzn_subcircuit_clpfd(S, IX, G, []).

% [MC] new for minizinc-2.0

% predicate maximum_arg_bool(array[int] of var bool: x, var int: i, int: base);
% predicate maximum_arg_int(array[int] of var int: x, var int: i, int: base);
fzn_constraint_clpfd(maximum_arg_sicstus, [[X]-M, I, B], Anns, G, [array(M,int), int, int], Anns) :-
	fzn_maximum_arg(X, I, B, G, []).

% predicate minimum_arg_bool(array[int] of var bool: x, var int: i, int: base);
% predicate minimum_arg_int(array[int] of var int: x, var int: i, int: base);
fzn_constraint_clpfd(minimum_arg_sicstus, [[X]-M, I, B], Anns, G, [array(M,int), int, int], Anns) :-
	fzn_minimum_arg(X, I, B, G, []).

fzn_constraint_clpfd(array_int_minimum, [X, [A]-M], Anns,
		     [minimum(X, A)], [int, array(M, int)], Anns).

fzn_constraint_clpfd(array_int_maximum, [X, [A]-M], Anns,
		     [maximum(X, A)], [int, array(M, int)], Anns).

% OBSOLETE?
fzn_constraint_clpfd(bool_clause_reif, [[A]-M, [B]-N, Y], Anns,
		     [scalar_product_reif(CsDs, AB, #>=, Lim, Y)],
		     [array(M, bool), array(N, bool), bool], Anns) :-
	(   foreach(_,A),
	    foreach(1,Cs)
	do  true
	),
	(   foreach(_,B),
	    foreach(-1,Ds)	    
	do  true
	),
	length(B, NB),
	Lim is 1-NB,
	append(Cs, Ds, CsDs),
	append(A, B, AB).

% predicate arg_sort_int(array[int] of var int: x, array[int] of var int: p, int: base);
fzn_constraint_clpfd(arg_sort_int_sicstus, [[X]-M, [P]-M, B], Anns, G, [array(M,int), array(M,int), int], Anns) :-
	fzn_arg_sort(X, P, B, G, []).

% predicate disjunctive(array[int] of var int: s,
%                       array[int] of var int: d);
fzn_constraint_clpfd(fzn_disjunctive,
		     [[Xs]-M, [Ds]-M],
		     Anns,
		     G,
		     [array(M,int), array(M,int)],
		     Anns) :-
	(   foreach(X,Xs),
	    foreach(D,Ds),
	    foreach([X-D],Items)
	do  true
	),
	G = [diffn(Items, [strict(false)])].

% predicate disjunctive_strict(array[int] of var int: s,
%                              array[int] of var int: d);
fzn_constraint_clpfd(fzn_disjunctive_strict,
		     [[Xs]-M, [Ds]-M],
		     Anns,
		     G,
		     [array(M,int), array(M,int)],
		     Anns) :-
	(   foreach(X,Xs),
	    foreach(D,Ds),
	    foreach([X-D],Items)
	do  true
	),
	G = [diffn(Items, [strict(true)])].

fzn_constraint_clpfd(alldiff_0_sicstus, [[Xs]-M, U], Anns, Expansion, [array(M, int), int], Anns) :-
	alldiff_0_expansion(Xs, U, Expansion, []).

% predicate geost_sicstus(
%                       int        : k           ,
%     array[int] of int        : rect_size   ,
%     array[int] of int        : rect_offset ,
%     array[int] of 0..1       : shape       , 
%     array[int] of var int    : x           ,
%     array[int] of var int    : kind        ,
%     array[int] of var int    : l           ,
%     array[int] of var int    : u
% );
% N = #objects
% M = #sboxes
% K = #dimensions
% S = #shapes
fzn_constraint_clpfd(geost_sicstus,
		     [K, [SIZE]-KM, [OFFSET]-KM, [SHAPE01]-SM, [ORIG]-KN, [KIND]-N, [L]-K, [U]-K],
		     Anns0,
		     G,
		     [int, array(KM,int), array(KM,int), array(SM,int), array(KN,int), array(N,int),
		      array(K,int), array(K,int)],
		     Anns) :-
	anns_geost(Anns0, Anns, Options, []),
	fzn_geost(K, SIZE, OFFSET, SHAPE01, ORIG, KIND, L, U, Options, G, []).

bool_cmp_goal(X, Y, B, N, Goal) :-
	X==1,
	fd_dom(Y, 0..1), !,
	nth1(N, [Y #= B, Y #\= B, B #= 0, Y #= B], Goal).
bool_cmp_goal(X, Y, B, N, Goal) :-
	X==0,
	fd_dom(Y, 0..1), !,
	nth1(N, [Y #\= B, Y #= B, Y #= B, B #= 1], Goal).
bool_cmp_goal(X, Y, B, N, Goal) :-
	Y==1,
	fd_dom(X, 0..1), !,
	nth1(N, [X #= B, X #\= B, X #\= B, B #= 1], Goal).
bool_cmp_goal(X, Y, B, N, Goal) :-
	Y==0,
	fd_dom(X, 0..1), !,
	nth1(N, [X #\= B, X #= B, B #= 0, X #\= B], Goal).

diffn_implied(X, Y, DX, DY, Options) -->
	(   {ground(DX)},
	    {ground(DY)}
	->  (   foreach(Xj, X),
		foreach(Yj, Y),
		foreach(DXj, DX),
		foreach(DYj, DY),
		foreach(Xminj, Xmin),
		foreach(Xmaxj, Xmax),
		foreach(Yminj, Ymin),
		foreach(Ymaxj, Ymax),
		foreach(task(Xj,DXj,_,DYj,I),Tasks1),
		foreach(task(Yj,DYj,_,DXj,I),Tasks2),
		count(I,1,_)
	    do  {fd_min(Xj, Xminj)},
		{fd_max(Xj, Xmaxj)},
		{fd_min(Yj, Yminj)},
		{fd_max(Yj, Ymaxj)}
	    ),
	    {min_member(Xinf, Xmin)},
	    {max_member(Xsup, Xmax)},
	    {min_member(Yinf, Ymin)},
	    {max_member(Ysup, Ymax)},
	    {max_member(W, DX)},
	    {max_member(H, DY)},
	    {Lim1 is H + Ysup - Yinf},
	    {Lim2 is W + Xsup - Xinf},
	    [cumulative(Tasks1, [limit(Lim1)|Options])],
	    [cumulative(Tasks2, [limit(Lim2)|Options])]
	;   []
	).

delete_zero_terms(As, Xs, A1s, X1s) :-
	(   foreach(A,As),
	    foreach(X,Xs),
	    fromto(A1s,A2s,A3s,[]),
	    fromto(X1s,X2s,X3s,[])
	do  (   X==0 -> A2s = A3s, X2s = X3s
	    ;           A2s = [A|A3s], X2s = [X|X3s]
	    )
	).

strength_reduce(scalar_product([0], [_], #=, 0, _), true) :- !.
strength_reduce(scalar_product([0], [_], #=, _, _), false) :- !.
strength_reduce(scalar_product([A], [X], #=, S, _), X #= Q) :-
	S mod A =:= 0, !,
	Q is S div A. 
strength_reduce(scalar_product([_], [_], #=, _, _), false) :- !.
strength_reduce(scalar_product([1,-1], [X,Y], #=, 0, _), X #= Y) :- !.
strength_reduce(Constraint, Constraint).

fzn_geost(K, SIZE, OFFSET, SHAPE01, ORIG, KIND, Lbox, Ubox, Options) -->
	{length(SIZE, KM),
	 M is KM//K},
	{   foreach(rect(J,Offset,Size),Rects),
	    fromto(SIZE,SIZE1,SIZE2,[]),
	    fromto(OFFSET,OFFSET1,OFFSET2,[]),
	    for(J,1,M),
	    param(K)
	do  length(Size, K),
	    append(Size, SIZE2, SIZE1),
	    length(Offset, K),
	    append(Offset, OFFSET2, OFFSET1)
	},
	{   fromto(SHAPE01,SHAPE02,SHAPE04,[]),
	    fromto(Sboxes1,Sboxes2,Sboxes5,[]),
	    count(S,1,_),
	    param(M,Rects)
	do  (   for(J1,1,M),
		fromto(SHAPE02,[Flag|SHAPE03],SHAPE03,SHAPE04),
		fromto(Sboxes2,Sboxes3,Sboxes4,Sboxes5),
		param(S,Rects)
	    do  (   Flag=:=0 -> Sboxes3 = Sboxes4
		;   Sboxes3 = [sbox(S,Offset1,Size1)|Sboxes4],
		    memberchk(rect(J1,Offset1,Size1), Rects)
		)
	    )
	},
	{   foreach(object(I,Kind,Orig),Objects),
	    foreach(Kind,KIND),
	    fromto(ORIG,ORIG1,ORIG2,[]),
	    count(I,1,_),
	    param(K)
	do  length(Orig, K),
	    append(Orig, ORIG2, ORIG1)
	},
	% (   foreach(Li,Lbox),
	%     foreach(Ui,Ubox),
	%     foreach(Lh,Lhull),
	%     foreach(Uh,Uhull)
	% do  [Li #=< Lh, Uh #=< Ui]
	% ),
	[geost(Objects, Sboxes1, [bounding_box(Lbox,Ubox)|Options])].

% U is the largest domain value that can occur
alldiff_0_expansion(Xs, U) -->
	(   foreach(X,Xs),
	    foreach(Y,Ys),
	    count(V,U+1,_)
	do  [clpfd:swap_values(X, Y, 0, V)]
	),
	[all_distinct(Ys)].

% XREF geost.mzn
anns_geost([geost_corners|Anns0], Anns) --> !, [corners(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_cumulative|Anns0], Anns) --> !, [cumulative(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_disjunctive|Anns0], Anns) --> !, [disjunctive(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_dynamic_programming|Anns0], Anns) --> !, [dynamic_programming(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_longest_hole|Anns0], Anns) --> !, [longest_hole(true,1000)],
	anns_geost(Anns0, Anns).
anns_geost([geost_pallet_loading|Anns0], Anns) --> !, [pallet_loading(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_parconflict|Anns0], Anns) --> !, [parconflict(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_polymorphism|Anns0], Anns) --> !, [polymorphism(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_task_intervals|Anns0], Anns) --> !, [task_intervals(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_visavis|Anns0], Anns) --> !, [visavis(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_visavis_floating|Anns0], Anns) --> !, [visavis_floating(true)],
	anns_geost(Anns0, Anns).
anns_geost([geost_visavis_init|Anns0], Anns) --> !, [visavis_init(true)],
	anns_geost(Anns0, Anns).
anns_geost(Anns, Anns) --> [].

anns_consistency_option(Anns0, Anns, Options) :-
	(   selectchk(domain, Anns0, Anns)
	->  Options = [consistency(domain)]
	;   selectchk(bounds, Anns0, Anns)
	->  Options = [consistency(bound)]
	;   selectchk(value, Anns0, Anns)
	->  Options = [consistency(value)]
	;   Anns = Anns0,
	    Options = []
	).

% [MC] 4.3.3: this is relevant for redundant cumulative constraints.
% Always supply global(true).  Don't add any "global" annotation.
% anns_global_option(Anns, Anns, [global(true)]).
anns_global_option(Anns0, Anns, Options) :- % [MC] 4.4
	(   selectchk(domain, Anns0, Anns)
	->  Options = [global(true)]
	;   Anns = Anns0,
	    Options = []
	).

relop_from_spec( -2, #<  ).
relop_from_spec( -1, #=< ).
relop_from_spec(  0, #=  ).
relop_from_spec(  1, #>= ).
relop_from_spec(  2, #>  ).

pairwise_relation(X, RelOpSpec, Goals) :-
	relop_from_spec(RelOpSpec, RelOp),
	X = [Y|Ys],
	pairwise_relation(Ys, Y, RelOp, Goals).

pairwise_relation([], _, _, []).
pairwise_relation([X|Xs], Y, RelOp, [G|Goals]) :-
	G =.. [RelOp, Y, X],
	pairwise_relation(Xs, X, RelOp, Goals).

fzn_element_clpfd(MinIndex, I, X, Y, [Xi = Y]) :-
	integer(I), !,
	I1 is I-MinIndex + 1,
	nth1(I1, X, Xi).
fzn_element_clpfd(1, I, X, Y, [element(I, X, Y)]) :- !.
fzn_element_clpfd(MinIndex, I, X, Y, [I1 #= I-MinIndex + 1, element(I1, X, Y)]).

gcc_add_unconstrained(Keys, X, UnconstrainedKeyCounts) :-
	list_to_fdset(Keys, Constrained),
	empty_fdset(Empty),
	(   foreach(Xj, X),
	    fromto(Empty, Un0, Un1, UnconstrainedInf),
	    param(Constrained)
	do  fd_set(Xj, Sj),
	    fdset_subtract(Sj, Constrained, SjUnconstrained),
	    fdset_union(Un0, SjUnconstrained, Un1)
	),
	prolog_flag(min_tagged_integer, Min),
	prolog_flag(max_tagged_integer, Max),
	fdset_interval(MinMax, Min, Max),
	fdset_intersection(MinMax, UnconstrainedInf, Unconstrained),
	fdset_to_list(Unconstrained, UnconstrainedKeys),
	(   foreach(UKey, UnconstrainedKeys),
	    foreach(UKey-_, UnconstrainedKeyCounts)
	do  true
	).

chop_array(Rowlen, List, Array) :-
	(   fromto(List,List1,List2,[]),
	    foreach(Row,Array),
	    param(Rowlen)
	do  length(Row, Rowlen),
	    append(Row, List2, List1)
	).

fzn_bin_packing_capa_clpfd(Capa, Bin, Weight, MinIndex) -->
	{length(Capa, M)},
	{length(Bin, N)},
	{length(Weight, N)},
	{MaxIndex is MinIndex+M-1},
	[domain(Bin, MinIndex, MaxIndex)],
	{   foreach(B,Bin),
	    foreach(W,Weight),
	    foreach(item(B,W),Items)
	do  true
	},
	(   foreach(C,Capa),
	    for(J,MinIndex,MaxIndex),
	    foreach(bin(J,L),Bins)
	do  [L in 0..C]
	),
	[bin_packing(Items, Bins)].

fzn_bin_packing_load_clpfd(Load, Bin, Weight, MinIndex) -->
	{length(Load, M)},
	{length(Bin, N)},
	{length(Weight, N)},
	{MaxIndex is MinIndex+M-1},
	[domain(Bin, MinIndex, MaxIndex)],
	{   foreach(B,Bin),
	    foreach(W,Weight),
	    foreach(item(B,W),Items)
	do  true
	},
	{   foreach(L,Load),
	    for(J,MinIndex,MaxIndex),
	    foreach(bin(J,L),Bins)
	do  true
	},
	[bin_packing(Items, Bins)].

fzn_bin_packing_clpfd(Capa, Bin, Weight) -->
	{   foreach(B,Bin),
	    foreach(W,Weight),
	    foreach(item(B,W),Items),
	    foreach(BFD,BFDs)
	do  fd_set(B, BFD)
	},
	{fdset_union(BFDs,UFD)},
	{fdset_to_list(UFD,UList)},
	(   foreach(J,UList),
	    foreach(bin(J,L),Bins),
	    param(Capa)
	do  [L in 0..Capa]
	),
	[bin_packing(Items, Bins)].

% fzn_count_clpfd(Xs, Y, Card) -->
% 	(   foreach(X,Xs),
% 	    foreach(B,Bs),
% 	    param(Y)
% 	do  [X #= Y #<=> B]
% 	),
% 	[sum(Bs, #=, Card)].

fzn_distribute_clpfd(Card, Value, Base, Options) -->
	(   foreach(B,Base),
	    foreach(Set,Sets)
	do  {fd_set(B, Set)}
	),
	{fdset_union(Sets, Union)},
	{fdset_to_list(Union, Values)},
	{   foreach(V,Values),
	    foreach(C,Cs),
	    foreach(V-C,Pairs)
	do  true
	},
	(   foreach(VVar,Value),
	    foreach(CVar,Card),
	    param(Union,Values,Cs,Pairs)
	do  (   {integer(VVar)} ->
		{memberchk(VVar-CVar, Pairs)}
	    ;   [VVar in_set Union],
		[element(I, Values, VVar)],
		[element(I, Cs, CVar)]
	    )
	),
	{length(Base, N)},
	[sum(Cs, #=, N)],
	[global_cardinality(Base, Pairs, Options)].

fzn_value_precede_clpfd(S, T, X) -->
	[value_precede_chain([S,T], X, [global(false)])].

fzn_value_precede_chain_clpfd(ST, X) -->
	[value_precede_chain(ST, X, [global(false)])].

fzn_seq_precede_chain_clpfd(X) -->
	[seq_precede_chain(X, [global(false)])].

fzn_regular_clpfd(X, _, S, D, Q0, F0) -->
	[automaton(X, [source(Q0)|Sinks], Arcs)],
	{type_to_fdset(F0, Fset)},
	{fdset_to_list(Fset, SinkNos)},
	{   foreach(Si,SinkNos),
	    foreach(sink(Si),Sinks)
	do  true
	},
	{   foreach(Di,D),
	    count(I,0,_),
	    fromto(Arcs,Arcs1,Arcs2,[]),
	    param(S)
	do  (   Di=:=0 -> Arcs1 = Arcs2
	    ;   Arcs1 = [arc(State,Letter,Di)|Arcs2],
		State is I//S + 1,
		Letter is I mod S + 1
	    )
	}.

fzn_cost_regular_clpfd(X, _, S, DD, Q0, F0, CC, C) -->
	[automaton(X, _, X, [source(Q0)|Sinks], Arcs, [Cost], [0], [C])],
	{type_to_fdset(F0, Fset)},
	{fdset_to_list(Fset, SinkNos)},
	{   foreach(Si,SinkNos),
	    foreach(sink(Si),Sinks)
	do  true
	},
	{   foreach(Di,DD),
	    foreach(Ci,CC),
	    count(I,0,_),
	    fromto(Arcs,Arcs1,Arcs2,[]),
	    param(S,Cost)
	do  (   Di=:=0 -> Arcs1 = Arcs2
	    ;   Arcs1 = [arc(State,Letter,Di,[Cost+Ci])|Arcs2],
		State is I//S + 1,
		Letter is I mod S + 1
	    )
	}.

fzn_mdd_clpfd(X, Level, From, Label, To) -->
	[case(Y, [X], Dag)],
	{length(X, N)},
	{length(Y, N)},
	{fzn_mdd_aux(Y, Level, From, Label, To, Dag)}.

fzn_cost_mdd_clpfd(X, Level, From, Label, To, Cost, TotalCost) -->
	{length(X, N)},
	{length(Y, N)},
	[sum(Y, #=, TotalCost)],
	{fzn_decostify_mdd(X, Y, Level, From, Label, To, Cost, XY1, Level1, From1, Label1, To1)},
	fzn_mdd_clpfd(XY1, Level1, From1, Label1, To1).

fzn_decostify_mdd(X, Y, Level, From, Label, To, Cost, XY1, Level1, From1, Label1, To1) :-
	length(Level, N),
	length(From, E),
	(   for(I,N+1,N+E),
	    foreach(I,NewNodes)
	do  true
	),
	(   foreach(Xi,X),
	    foreach(Yi,Y),
	    fromto(XY1,[Xi,Yi|XY2],XY2,[])
	do  true
	),
	(   foreach(L,Level),
	    foreach(L1,Level1a)
	do  L1 is 2*L-1
	),
	(   foreach(F,From),
	    foreach(L2,Level1b),
	    param(Level)
	do  nth1(F, Level, F1),
	    L2 is 2*F1
	),
	append(Level1a, Level1b, Level1),
	(   foreach(Ci,Cost),
	    foreach(range(Ci,Ci),Cost1)
	do  true
	),
	append(Label, Cost1, Label1),
	append(From, NewNodes, From1),
	append(NewNodes, To, To1).	

fzn_mdd_aux(X, Level, From, Label, To, Nodes) :-
	(   foreach(Fi,From),
	    foreach(Li,Label),
	    foreach(Ti,To),
	    fromto(KEdges1,KEdges2,KEdges5,[])
	do  fzn_to_prolog(Li, FDSet),
	    (   foreach([A|B],FDSet),
		fromto(KEdges2,KEdges3,KEdges4,KEdges5),
		param(Ti,Fi)
	    do  (Ti = 0 -> KEdge = Fi-(A..B) ; KEdge = Fi-((A..B)-Ti)),
		KEdges3 = [KEdge|KEdges4]
	    )
	),
	keysort(KEdges1, KEdges6),
	keyclumped(KEdges6, KEdges7),
	(   foreach(Lj,Level),
	    foreach(node(ID,Xi,Edges),Nodes),
	    count(ID,1,_),
	    param(X,KEdges7)
	do  (   nth1(Lj, X, Xi),
		memberchk(ID-Edges, KEdges7)
	    )
	).


% predicate among(var int: n, array[int] of var int: x, set of int: v)
fzn_among_clpfd(N, X, V) -->
	{type_to_fdset(V, Vset)},
	(   foreach(Xi,X),
	    foreach(B,Bs),
	    param(Vset)
	do  [Xi in_set Vset #<=> B]
	),
	[sum(Bs, #=, N)].

% [MC] new for minizinc-1.5
fzn_circuit_clpfd(Ss, IX) -->
	{length(Ss, N)},
	{set_values(IX, Values)},
	(   {min_member(1, Values)},
	    {max_member(N, Values)}
	->  [circuit(Ss)]
	;   (   foreach(S,Ss),
		foreach(Y,Ys),
		foreach(V,Values),
		foreach([S,Y],Pairs),
		foreach([V,I],Map),
		count(I,1,_)
	    do  []
	    ),
	    [domain(Ys,1,N)],
	    [circuit(Ys)],
	    [table(Pairs,Map)]
	).

fzn_subcircuit_clpfd(Ss, IX) -->
	{length(Ss, N)},
	{set_values(IX, Values)},
	(   {min_member(1, Values)},
	    {max_member(N, Values)}
	->  [subcircuit(Ss)]
	;   (   foreach(S,Ss),
		foreach(Y,Ys),
		foreach(V,Values),
		foreach([S,Y],Pairs),
		foreach([V,I],Map),
		count(I,1,_)
	    do  []
	    ),
	    [domain(Ys,1,N)],
	    [subcircuit(Ys)],
	    [table(Pairs,Map)]
	).

% [MC] new for minizinc-2.0

fzn_maximum_arg(Xs, I, B) -->
	[maximum_arg(Xs, J)],
	(   {B=1} -> [I=J]
	;   [I #= J+B-1]
	).

fzn_minimum_arg(Xs, I, B) -->
	[minimum_arg(Xs, J)],
	(   {B=1} -> [I=J]
	;   [I #= J+B-1]
	).

fzn_arg_sort(Xs, Ps, 1) --> !,
	{   foreach(X,Xs),
	    foreach([X],Ys),
	    foreach([_],Zs)
	do  true
	},
	[keysorting(Ys, Zs, [permutation(Ps)])].
fzn_arg_sort(Xs, Ps, B) -->
	{Offset is B-1},
	(   foreach(X,Xs),
	    foreach([X],Ys),
	    foreach([_],Zs),
	    foreach(P,Ps),
	    foreach(Q,Qs),
	    param(Offset)
	do  [P #= Q+Offset]
	),
	[keysorting(Ys, Zs, [permutation(Qs)])].

%%%%% Solve %%%%%

interpret_solve(satisfy, Anns, State0, RestAnns, State) :-
	state_get([time_out], State0, [TO]),
 	fzn_search_clpfd(Anns, State0, State1, [], RestAnns, SubGoals, []),
	% (   Found = []
	% ->  Increment = true
	% ;   Increment = fzn_increment_counter(Found)
	% ),
	Goal = (solve([satisfy|TO],SubGoals), fzn_output(State)),
	state_set([solve], State1, [Goal], State).
interpret_solve(minimize(Expr0), Anns, State0, RestAnns, State) :-
	state_get([table, solutions, search, time_out], State0,
		  [Table0, Solutions, Search, TO]),
	(Solutions = all -> Flag = all ; Flag = best),
	type(Expr0, Table0, int),
	deref(Expr0, Table0, Expr),
 	fzn_search_clpfd(Anns, State0, State1, Expr, RestAnns, SubGoals, []),
	% (   Found = []
	% ->  Increment = true
	% ;   Increment = fzn_increment_counter(Found)
	% ),
	Goal = (solve([minimize(Expr),Flag,Search|TO],SubGoals), fzn_output(State)),
	state_set([solve], State1, [Goal], State).
interpret_solve(maximize(Expr0), Anns, State0, RestAnns, State) :-
	state_get([table, solutions, search, time_out], State0,
		  [Table0, Solutions, Search, TO]),
	(Solutions = all -> Flag = all ; Flag = best),
	type(Expr0, Table0, int),
	deref(Expr0, Table0, Expr),
	fzn_search_clpfd(Anns, State0, State1, Expr, RestAnns, SubGoals, []),
	% (   Found = []
	% ->  Increment = true
	% ;   Increment = fzn_increment_counter(Found)
	% ),
	Goal = (solve([maximize(Expr),Flag,Search|TO],SubGoals), fzn_output(State)),
	state_set([solve], State1, [Goal], State).

%% add any omitted vars to search
fzn_search_clpfd(Anns, State0, State, _Precious, RestAnns) -->
	{state_get([vars, free_search], State0, [VarBag, FS])},
	{sort(VarBag, VarSet)},
	{state_set([vars], State0, [VarSet], State)},
	{search_anns(Anns, State, RestAnns, SubGoals0, [])},
	{FS = false -> SubGoals = SubGoals0 ; SubGoals = []},
	{term_variables_set(SubGoals, LabeledSet)},
	{ord_subtract(VarSet, LabeledSet, RestSet)},
	{   foreach(X,RestSet),
	    fromto(BoolVars1,BoolVars2,BoolVars3,[]),
	    fromto(FDVars1,FDVars2,FDVars3,[])
	do  fd_set(X, FDSet),
	    (   FDSet = [[0|1]]
	    ->  BoolVars2 = [X|BoolVars3],
		FDVars2 = FDVars3
	    ;   BoolVars2 = BoolVars3,
		FDVars2 = [X|FDVars3]
	    )
	},
	(   foreach(SubGoal,SubGoals)
	do  [SubGoal]
	),
	(   {FDVars1 = []} -> []
	;   [labeling([dom_w_deg,bisect],FDVars1)]
	),
	(   {BoolVars1 = []} -> []
	;   [labeling([dom_w_deg],BoolVars1)]
	).

search_anns([], _, []) --> [].
search_anns([A|As], State0, RestAnns) -->
	{A = seq_search(As0)}, !,
	search_anns(As0, State0, RestAnns),
	search_anns(As, State0, RestAnns).
search_anns([A|As], State0, RestAnns) -->
	search_ann(A, State0), !,
	search_anns(As, State0, RestAnns).
search_anns([A|As], State0, [A|RestAnns]) -->
	search_anns(As, State0, RestAnns).

search_ann(Ann, State0) -->
	{bool_or_int_search(Ann, Xs0, VarSelect, ValueChoice0, ExploreMethod0)}, !,
	{state_get([table], State0, [Table0])},
	{deref(Xs0, Table0, Xs1)},
	{array_values(Xs1, Xs)},
	{length(Xs, L)},
	{type(Xs0, Table0, array(L, int))},
	{value_choice(ValueChoice0, ValueChoice)},
	{explore_method(ExploreMethod0, ExploreMethod)},
	{append([VarSelect | ValueChoice], ExploreMethod, Options)},
	[labeling(Options, Xs)].

bool_or_int_search(int_search(Xs, VarSelect, ValueChoice, ExploreMethod),
		   Xs, VarSelect, ValueChoice, ExploreMethod).
bool_or_int_search(bool_search(Xs, VarSelect, ValueChoice, ExploreMethod),
		   Xs, VarSelect, ValueChoice, ExploreMethod).

value_choice(indomain, [enum, up]).
value_choice(indomain_min, [step, up]).
value_choice(indomain_max, [step, down]).
value_choice(indomain_middle, [step, middle]).
value_choice(indomain_median, [step, median]).
value_choice(indomain_random, [value(step_values(fd_random))]).
value_choice(indomain_split, [bisect, up]).
value_choice(indomain_reverse_split, [bisect, down]).
value_choice(indomain_interval,
	     [value(step_values(fd_indomain_interval))]).
value_choice(outdomain_min, [value(step_values(fd_outdomain_min))]).
value_choice(outdomain_max, [value(step_values(fd_outdomain_max))]).

% Used in translated code.
:- public step_values/5.
:- step_values/5 is nondet.
step_values(ValuePred, X, _L, BB0, BB) :-
        % [PM] 4.2.1 Make module explicit (and avoid SPIDER warning).
        %      If you think this is slow then get rid of call/3 first.
        my_module(M),
	call(M:ValuePred, X, Set),
	try_value(X, Set, BB0, BB).

:- try_value/4 is nondet.
try_value(X, Set, BB0, BB) :-
	X in_set Set,
	first_bound(BB0, BB).
try_value(X, Set0, BB0, BB) :-
	fd_set(X, Set),
	fdset_subtract(Set, Set0, Set1),
	X in_set Set1,
	later_bound(BB0, BB).

% Used in translated code.
:- public fd_random/2.
fd_random(X, RSet) :-
	fd_set(X, S),
	fdset_to_list(S, Vs),
	random_member(R, Vs),
	fdset_singleton(RSet, R).

% Used in translated code.
:- public fd_indomain_interval/2.
fd_indomain_interval(X, I) :-
	fd_set(X, S),
	fdset_interval(S, L, U), !,
	Mid is L+(U-L)//2,
	fdset_interval(I, L, Mid).
fd_indomain_interval(X, I) :-
	fd_set(X, S),
	fdset_parts(S, L, U, _),
	fdset_interval(I, L, U).	


% Used in translated code.
:- public fd_outdomain_min/2.
fd_outdomain_min(X, S) :-
	fd_set(X, S),
	fdset_singleton(S, _), !.
fd_outdomain_min(X, S) :-
	fd_min(X, M),
	fd_set(X, S0), 
	fdset_del_element(S0, M, S).

% Used in translated code.
:- public fd_outdomain_max/2.
fd_outdomain_max(X, S) :-
	fd_set(X, S),
	fdset_singleton(S, _), !.
fd_outdomain_max(X, S) :-
	fd_max(X, M),
	fd_set(X, S0), 
	fdset_del_element(S0, M, S).


explore_method(complete, []) :- !. % [MC] NOT [all]; it disables minimize/maximize!
explore_method(fail, [fail]) :- !.
%explore_method(Lds, [discrepancy(D)]) :-
%	      append(["lds(", C, ")"], Lds), !,
%	      number_codes(D, C).

%%%%% Output %%%%%

expr_to_codes(X, _, _) :-
	var(X), !, 
	illarg(var, expr_to_codes(X, _, _), 1).
expr_to_codes(false, _, "false") :- !.
expr_to_codes(true, _, "true") :- !.
expr_to_codes(range(L0, U0), Table, Codes) :- !,
	deref(L0, Table, L),
	deref(U0, Table, U),
	format_to_codes("~d..~d", [L, U], Codes).
expr_to_codes(Id, Table, Codes) :-
	atom(Id), % identifier(Id),
	!,
	table_lookup(Id, Table, Value0, Type),
	deref(Value0, Table, Value),
	(   booltype(Type)
	->  bool_expr_to_codes(Value, Codes)
	;   expr_to_codes(Value, Table, Codes)
	).
expr_to_codes(N, _, Codes) :-
	number(N), !,
	number_codes(N, Codes).
expr_to_codes(Set, Table0, Codes) :-
	set_values(Set, Values), !,
	expr_to_codes_list(Values, Table0, Codes0),
	append(["{", Codes0, "}"], Codes).
expr_to_codes(Array, Table0, Codes) :-
	array_values(Array, Values), !,
	expr_to_codes_list(Values, Table0, Codes0),
	append(["[", Codes0, "]"], Codes).

expr_to_codes_list([], _, []).
expr_to_codes_list([V], Table0, Codes) :- !,
	expr_to_codes(V, Table0, Codes).
expr_to_codes_list([V|Vs], Table0, Codes) :-
	expr_to_codes(V, Table0, Codes0),
	expr_to_codes_list(Vs, Table0, Codes1),
	append([Codes0, ", ", Codes1], Codes).

booltype(bool).
booltype(array(_, bool)).
booltype(set(bool)).

bool_expr_to_codes(X, _) :-
	var(X), !,
	illarg(var, expr_to_codes(X, _, _), 1).
bool_expr_to_codes(0, "false") :- !.
bool_expr_to_codes(1, "true") :- !.
bool_expr_to_codes(false, "false") :- !.
bool_expr_to_codes(true, "true") :- !.
bool_expr_to_codes(Set, Codes) :-
	set_values(Set, Values), !,
	bool_expr_to_codes_list(Values, Codes0),
	append(["{", Codes0, "}"], Codes).
bool_expr_to_codes(Array, Codes) :-
	array_values(Array, Values), !,
	bool_expr_to_codes_list(Values, Codes0),
	append(["[", Codes0, "]"], Codes).

bool_expr_to_codes_list([], []).
bool_expr_to_codes_list([B], Codes) :- !,
	bool_expr_to_codes(B, Codes).
bool_expr_to_codes_list([B|Bs], Codes) :-
	bool_expr_to_codes(B, Codes0),
	bool_expr_to_codes_list(Bs, Codes1),
	append([Codes0, ", ", Codes1], Codes).

%%%%% FlatZinc types %%%%%

deref(X, _, X) :-
	var(X), !.
deref(true, _, 1) :- !.    % since we treat bools as 0..1 variables
deref(false, _, 0) :- !.   % since we treat bools as 0..1 variables
deref(range(L0, U0), Table, range(L, U)) :- !,
	deref(L0, Table, L),
	deref(U0, Table, U).
deref(X, _, X) :-
	number(X), !.
deref(Subscript, Table, Value) :-
	subscript(Subscript, Id, Index0), !,
	deref(Index0, Table, Index),
	table_lookup_value(Id, Table, Array),
	array_get(Index, Array, Value0),
	deref(Value0, Table, Value).
deref(Array0, Table, Array) :-
	array_values(Array0, Values0), !,
	deref_list(Values0, Table, Values),
	array_values(Array, Values).
deref(Id, Table, Value) :-	% [MC] 4.3: 'empty' used to be both AVL and identifier
	atom(Id), % identifier(Id),
	!,
	table_lookup_value(Id, Table, Value0),
	deref(Value0, Table, Value).
deref(Set0, Table, Set) :-
	set_values(Set0, Values0), !,
	deref_list(Values0, Table, Values1),
	sort(Values1, Values),
	set_values(Set, Values).
deref(Primitive, _, Primitive).

deref_list([], _, []).
deref_list([V0|VS0], Table, [V|VS]) :-
	deref(V0, Table, V),
	deref_list(VS0, Table, VS).

%%%%% Typing %%%%%

type(V, Table, T) :-
	type_quiet(V, Table, T), !.
type(V, Table, T) :-
	illarg(type(typeof(V, T)), type(V, Table, T), 1).

:- type_quiet/3 is nondet.
type_quiet(X, _Table, T) :-
	var(X),
	type_to_fdset(T, S),
	X in_set S.
	% [MC] 4.2.1 benchmark prize_collection barfs here. Relax for now.
	% (   (fd_set(X, S0), type_to_fdset(T, S1), fdset_subset(S0, S1))
	% ->  true
	% ;   illarg(type(typeof(X, T)), type(fdvar, Table, T), 1)	    
	% ).
type_quiet(false, _, T) :-
	type_to_fdset(T, S),
	fdset_member(0, S).
type_quiet(true, _, T) :-
	type_to_fdset(T, S),
	fdset_member(1, S).
type_quiet(range(L0, U0), Table, set(T)) :-
	deref(L0, Table, L),
	deref(U0, Table, U),
	integer(L),
	integer(U),
	fdset_interval(S0, L, U),
	type_to_fdset(T, S),
	fdset_subset(S0, S).
type_quiet(Subscript, Table, T) :-
	Subscript = '_subscript'(_Id,_Index), % subscript(Subscript, _Id, _Index)
	deref(Subscript, Table, Value),
	type(Value, Table, T).
type_quiet(Array, Table, array(L, T)) :-
	Array = [Values],	% array_values(Array, Values),
	length(Values, L),
	type_list(Values, Table, T).
type_quiet(Set, Table, set(T)) :-
	set_values(Set, Values),
	type_list(Values, Table, T).
type_quiet(Id, Table, T) :-
	atom(Id), % identifier(Id),
	deref(Id, Table, Value),
	type(Value, Table, T).
type_quiet(I, _Table, T) :-
	integer(I), 
	type_to_fdset(T, S),
	fdset_member(I, S).
type_quiet(F, _Table, T) :-
	float(F), 
	type_to_range(T, R),
	range_member(F, R).
type_quiet(S, _Table, string) :-
	(   foreach(MX,S)
	do  non_negative(MX)
	).

:- type_list/3 is nondet.
type_list([], _, _).
type_list([V|Vs], Table, T) :-
	type_quiet(V, Table, T),
	type_list(Vs, Table, T).

type_to_codes(bool, "bool") :- !.
type_to_codes(int, "int") :- !.
type_to_codes(float, "float") :- !.
type_to_codes(string, "string") :- !.
type_to_codes(set(T), S) :- !,
	type_to_codes(T, S0),
	append("set of ", S0, S).
type_to_codes(array(L, T), S) :- !,
	type_to_codes(T, S0),
	format_to_codes('array[1..~d] of ~s', [L, S0], S).
type_to_codes(T, S) :-
	expr_to_codes(T, S).

subtype(V, T1, T2) :-
	subtype_quiet(V, T1, T2), !.
subtype(V, T1, T2) :-
	illarg(type(typeof(V, T2)), subtype(V, T1, T2), 1).

:- subtype_quiet/3 is nondet.
subtype_quiet(E, set(T1), set(T2)) :-
	subtype(E, T1, T2).
subtype_quiet(E, array(L, T1), array(L, T2)) :-
	subtype(E, T1, T2).
subtype_quiet(_E, T1, T2) :-
	type_to_range(T1, range(L, U)),
	type_to_range(T2, R2),
	range_member(L, R2),
	range_member(U, R2).
subtype_quiet(_, T1, T2) :-
	type_to_fdset(T1, S1),
	type_to_fdset(T2, S2),
	fdset_subset(S1, S2).

type_to_domain(int, Int) :- !,
	range_to_fdset(inf..sup, Int).
type_to_domain(T, D) :-
	type_to_fdset(T, D).	

type_to_fdset(bool, Bool) :- !,
	range_to_fdset(0..1, Bool).
type_to_fdset(int, Int) :- !,
	range_to_fdset(inf..sup, Int).
type_to_fdset(range(Inf, Sup), Range) :- !,
	integer(Inf),
	integer(Sup),
	range_to_fdset(Inf..Sup, Range).
type_to_fdset(Set, S) :-
	set_values(Set, Values),
	(   foreach(MX,Values)
	do  integer(MX)
	),	
	list_to_fdset(Values, S).

type_to_range(float, range(inf, sup)).
type_to_range(range(L, U), range(L, U)) :-
	float(L),
	float(U).

range_member(_, range(inf, sup)) :- !.
range_member(F, range(inf, U)) :- !,
	F =< U.
range_member(F, range(L, sup)) :- !,
	F >= L.
range_member(F, range(L, U)) :-
	L =< F, F =< U.

expr_to_codes(X, "<unknown>") :-
	var(X), !.
expr_to_codes(false, "false") :- !.
expr_to_codes(true, "true") :- !.
expr_to_codes(Subscript, S) :-
	subscript(Subscript, Id, Index), !,
	format_to_codes('~q[~q]', [Id, Index], S).
expr_to_codes(Array, S) :-
	array_values(Array, Values), !,
	expr_to_codes_list(Values, S0),
	append(["[", S0, "]"], S).
expr_to_codes(range(L, U), S) :-
	integer(L),
	integer(U), !,
	format_to_codes('~d..~d', [L, U], S).
expr_to_codes(range(L, U), S) :-
	float(L),
	float(U), !,
	format_to_codes('~f..~f', [L, U], S).
expr_to_codes(Set, S) :-
	set_values(Set, Values), !,
	expr_to_codes_list(Values, S0),
	append(["{", S0, "}"], S).
expr_to_codes(Id, S) :-	% [MC] 4.3: 'empty' used to be both AVL and identifier
	identifier(Id), !,
	atom_codes(Id, S).
expr_to_codes(String, String) :-
	(   foreach(MX,String)
	do  non_negative(MX)
	), !.
expr_to_codes(E, S) :-
	format_to_codes('~q', [E], S).

expr_to_codes_list([], []).
expr_to_codes_list([E], S) :- !,
	expr_to_codes(E, S).
expr_to_codes_list([E|Es], S) :-
	expr_to_codes(E, S0),
	expr_to_codes_list(Es, S1),
	append([S0, ", ", S1], S).


%%%%% Annotations %%%%%
anns_not_supported([]).
anns_not_supported([A|AS]) :-
	A =.. [Name|Args0],
	expr_to_codes_list(Args0, Args),
	(   Args = []
	->  warning("% annotation `~q' ignored~n", [Name])
	;   warning("% annotation `~q(~s)' ignored~n", [Name, Args])
	),
	anns_not_supported(AS).

%%%%% Error handling %%%%%
%% FIXME: Error handling should use message hooks instead of writing to user_error.

warnings(off).

warning(Control, Arguments) :-
	(   warnings(on)
	->  format(user_error, Control, Arguments)
	;   true
	).

error_handler(Goal, Stream) :-
        E = _, % suppress SPIDER warning about catch all exception handler
	on_exception(E, Goal, ( % [PM] 4.2.1 time_out is no longer caught by on_exception/3
                                %     E = time_out ->  raise_exception(E)
			        % ;   otherwise ->
                                  line_count(Stream, L0),
				  L is L0 + 1,
				  format(user_error,
					 '! Item ending on line ~d:', [L]),
				  throw(E)
			      )).

:- multifile user:generate_message_hook/3.
% FlatZinc errors
user:generate_message_hook(Error) -->
	{Error = existence_error(_, _, identifier, Id, _)}, !,
	['Existence error'-[], nl,
	 '\`~q\' is not defined'-[Id], nl].
user:generate_message_hook(Error) -->
	{Error = existence_error(_, _, constraint, ConstraintTerm, _)}, !,
	['Existence error'-[], nl,
	 '\`~q\' is not defined'-[ConstraintTerm], nl].
user:generate_message_hook(Error) -->
	{Error = consistency_error(table_insert(_, _, _, _, _),
				   Id-(Value0, Type0),
				   Id-(Value, Type), _)}, !,
	{expr_to_codes(Value0, Value0Codes), type_to_codes(Type0, Type0Codes),
	 expr_to_codes(Value, ValueCodes), type_to_codes(Type, TypeCodes)},
	['Consistency error: \`~q\' is already defined'-[Id], nl,
	 'previous definition of ~q was \`~s : ~q = ~s\''-[Id, Type0Codes, Id,
							   Value0Codes], nl,
	 'cannot redefine ~q to \`~s : ~q = ~s\''-[Id, TypeCodes, Id,
						   ValueCodes], nl].
user:generate_message_hook(type_error(Goal, _, typeof(E, T), _)) -->
	{type_or_subtype(Goal)}, !,
	{expr_to_codes(E, EC), type_to_codes(T, TC)},
	['Type error'-[], nl,
	 '\`~s\' must be a member of \`~s\''-[EC, TC], nl].
user:generate_message_hook(type_error(Goal, _, between(L, U), _)) -->
	{Goal = array_get(Index, _, _)}, !,
	['Type error in array index'-[], nl,
	 'index evaluates to ~q but must be in ~q..~q'-[Index, L, U], nl].
user:generate_message_hook(type_error(Goal, _, infinite, _)) -->
	{Goal = fzn_labeling(_, _)}, !,
	['Type error'-[], nl,
	 'some variables in solve item have infinite domains'-[], nl].
user:generate_message_hook(instantiation_error(expr_to_codes(_, _, _), 1)) --> !,
	['Instantiation error'-[], nl,
	 'some output variables are not instantiated'-[], nl].
% top-level errors
user:generate_message_hook(Error) -->
	{Error = existence_error(Goal, Arg, objective, objective, _)}, !,
	['Existence error in argument ~d of fzn_objective/2'-[Arg], nl,
	 'there is no objective in this FlatZinc state'-[], nl,
	 'goal:  ~q'-[Goal], nl].
user:generate_message_hook(type_error(Goal, Arg, fzn_solution_arg, _)) --> !,
	['Type error in argument ~q of ~q'-[Arg, Goal], nl,
	 'number of solutions must be greater than 0 or the atom \`all\''-[],
	 nl, 'goal:  ~q'-[Goal], nl].

type_or_subtype(type(_, _, _)).
type_or_subtype(subtype(_, _, _)).

%%%%% Misc %%%%%

non_negative(I) :-
	integer(I),
	I >= 0.

identifier(E) :- atom(E).

subscript('_subscript'(Id, Index), Id, Index).

array_values([Values], Values).

set_values({Values}, Values) :- !.
set_values(range(Inf, Sup), Values) :-
	(   for(I, Inf, Sup),
	    foreach(I, Values)
	do  true
	).

andify([], true).
andify([G], G) :- !.
andify([G1, G2|Goals], (G1, Conj1)) :- 
	andify([G2|Goals], Conj1).
