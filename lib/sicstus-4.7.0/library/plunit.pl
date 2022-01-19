/*  Purpose:       Unit Test Harness
    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006-2008, University of Amsterdam
    Modified:      2010 by SICS for SICStus Prolog 4

    This file is covered by the `The Artistic License', also in use by
    Perl.  See http://www.perl.com/pub/a/language/misc/Artistic.html
*/

:- module(plunit, [
	  begin_tests/1,	% +Name
	  begin_tests/2,	% +Name, +Options
	  end_tests/1,		% +Name
	  run_tests/0,		%
	  run_tests/1,		% +Specs
	  run_tests/2		% +Specs, +Options
	  % set_test_options/1	% SWI specific
	  % load_test_files/1	% SWI specific
	  % running_tests/0	% SWI specific
	  % test_report/1	% SWI specific
	  ]).

:- use_module(library(ordsets), [ord_intersection/3,list_to_ord_set/2]).

:- use_module(library(lists), [select/3]).

:- use_module(library(types), [illarg/3,illarg/4,must_be/4]).

:- meta_predicate
        valid_options(+, -, 1, +, +),
        call_det(0, -).

:- multifile
	user:term_expansion/6,
	user:generate_message_hook/3,
	'SU_messages':generate_message/3.

:- dynamic
	enabled/0,		% enabler for hooks
	loading_unit/4,		% Unit, Module, File, OldSource
	current_unit/4,		% Unit, Module, File, Options
	passed/5,		% Unit, Name, Line, Det, Time
	failed/4,		% Unit, Name, Line, Error
	skipped/3,		% Unit, Name, Line
	blocked/4,		% Unit, Name, Line, Reason
	fixme/5,		% Unit, Name, Line, Reason, Ok
	seen_unit/1.
:- volatile
        % enabled/0,              % enabler for hooks
        loading_unit/4,         % Unit, Module, File, OldSource
        current_unit/4,         % Unit, Module, File, Options
        passed/5,               % Unit, Name, Line, Det, Time
        failed/4,               % Unit, Name, Line, Error
	skipped/3,		% Unit, Name, Line
        blocked/4,              % Unit, Name, Line, Reason
        fixme/5,                % Unit, Name, Line, Reason, Ok
	seen_unit/1.

:- loading_unit/4 is nondet.
:- current_unit/4 is nondet.
:- passed/5 is nondet.
:- failed/4 is nondet.
:- skipped/3 is nondet.
:- blocked/4 is nondet.
:- fixme/5 is nondet.
:- seen_unit/1 is nondet.

:- public
        test_option/1,
        test_set_option/1,
        passed/5,
        failed/4,
	skipped/3,
        run_option/1.


% See library(plunit) in the manual for details
:- Module:test/1 is hook.


% See library(plunit) in the manual for details
:- Module:test/2 is hook.


		 /*******************************
		 *	      MODULE		*
		 *******************************/


%%	begin_tests(+UnitName:atom) is det.
%%	begin_tests(+UnitName:atom, Options) is det.
%
%	Start a test-unit. UnitName is the  name   of  the test set. the
%	unit is ended by :- end_tests(UnitName).
:- begin_tests/1 is det.
begin_tests(Unit) :-
	begin_tests(Unit, []).

:- begin_tests/2 is det.
:- begin_tests/2 is documented_as(begin_tests/1).
begin_tests(Unit, Options) :-
	prolog_load_context(module, Module),
	prolog_load_context(source, Source),
	Excp = error(_,_),
	on_exception(Excp,
		     (   valid_options(Options, Option1, test_set_option, begin_tests(Unit,Options), 2),
			 canonicalize_test_set_options(Option1, Options3)
		     ),
		     (
			 % Ensure the set of tests is not run, but instead (once again) reports the problem with the options.
			 % By re-throwing the exception we will get a more descriptive text ("expected option, but found a_bad_set_option(bork)").
			 % By using blocked/1 would get a more discreet indication that something is wrong. I do not think
			 % discreet is disirable for malformed tests.
			 Options3 = [setup(throw(Excp))],
			 % Options = [blocked('Bad options')],
			 print_message(error, Excp)
		     )),
	(   current_unit(Unit, Module, Source, Options3) ->
	    true
	;   retractall(current_unit(Unit, Module, _, _)),
	    assertz(current_unit(Unit, Module, Source, Options3))
	),
	asserta(loading_unit(Unit, Module, Source, -)).


% Convert multiple occurrences of some options to conjuncts (or disjuncts), as appropriate. SPRM-20365.
% xref test_set_option/1
canonicalize_test_set_options(Options1, Options) :-
	% Multiple occurrences of these should be treated as a conjunct
	% E.g. [setup(open_input_file(In)),...,setup(open_output_file(Out)),...] ==> [setup((open_input_file(In)),setup(open_output_file(Out)), ...,...]
	merge_options_conjunct(Options1, condition, Options2),
	merge_options_conjunct(Options2, setup, Options3),
	merge_options_conjunct(Options3, cleanup, Options4),
	% Multiple occurrences of blocked/1 or fixme/1 are left as-as (the first one will be used).
	Options = Options4.


% Convert multiple occurrences of some options to conjuncts (or disjuncts), as appropriate. SPRM-20365.
% xref test_option/1
canonicalize_test_options(Options1, Options) :-
	% Unfortunately, we must can not merge true(A),true(B) to true((A,B)) here. It is handled elsewhere, instead.
	canonicalize_test_set_options(Options1, Options).


%%	end_tests(+Name) is det.
%
%	Close a unit-test module.
%
%	@tbd	Run tests/clean module?
%	@tbd	End of file?
:- end_tests/1 is det.
end_tests(Unit) :-
	loading_unit(StartUnit, _, _, _), !,
	(   Unit == StartUnit
	->  retractall(loading_unit(StartUnit, _, _, _))
	;   illarg(context(in_plunit(StartUnit),end_tests(Unit)), end_tests(Unit), 1)
	).
end_tests(Unit) :-
	illarg(context(not_in_plunit,end_tests(Unit)), end_tests(Unit), 1).


unit_module(Unit, Module) :-
	current_unit(Unit, Module, _, _), !.


                /*******************************
                *           EXPANSION          *
                *******************************/


%% expand_test(+Name, +Options, +Body, HeadBodyLay -Expansion, -ExpansionLay) is det.
%
% Expand test(Name, Options) :- Body  into a clause for
% 'unit test'/5 and 'unit body'/4.
expand_test(Name, Options0, Body, HeadBodyLay, Expansion, ExpansionLay) :-
	loading_unit(Unit, _, _, _),
	!,
	DummyLay = 0,                       % Unknown layout. Differs from '[]' which means (sticky) _no_ layout.
	% HeadBodyLay is for test(...) :- Body
	decomp_layout2(HeadBodyLay, HeadLay, BodyLay),
	condense_layout(HeadLay, HeadLine), % First line of the test(...) head
	condense_layout(BodyLay, BodyLine), % First line of the test body
	%
	UnitTest = 'unit test'(Unit,Name,File:Line,Options,Module:UnitBodyHead),
	UnitTestLay = DummyLay,             % no source info
	%
	UnitBodyHead = 'unit body'(Unit,Name,File:Line,Vars),
	UnitBodyHeadLay = HeadLine,         % pretend UnitBodyHead is at the same line as the test(...) head
	%
	Cut = (!),
	CutLay = BodyLine,                  % pretend the cut is at the same line as the first line of the body
	%
	CutBody = (Cut,Body),
	comp_layout2(BodyLay, CutLay, BodyLay, CutBodyLay),
	%
	UnitBody = (UnitBodyHead:-CutBody),
	comp_layout2(UnitBodyHeadLay, UnitBodyHeadLay, CutBodyLay, UnitBodyLay),
	%
	ExpansionTail2 = [],
	ExpansionTail2Lay = [],             % no source-info
	ExpansionTail1 = [UnitBody|ExpansionTail2],
	comp_layout2(UnitBodyLay, UnitBodyLay, ExpansionTail2Lay, ExpansionTail1Lay),
	Expansion = [UnitTest|ExpansionTail1],
	comp_layout2(DummyLay, UnitTestLay, ExpansionTail1Lay, ExpansionLay),
	%
	source_location(File, Line),
	prolog_load_context(module, Module),
	% We must extract only those variables common with Options so we do not mistakenly expose do/2 body
	% variables outside the loop which would give warnings (see SPRM-20640 for a discussion).
	(   term_variables(Body, VarList1),
	    list_to_ord_set(VarList1, VarSet),
	    term_variables(Options0, OptionsVarList1),
	    list_to_ord_set(OptionsVarList1, OptionsVarSet),
	    ord_intersection(OptionsVarSet, VarSet, VarList)
	),
	(   length(VarList, NVars),
	    NVars =< 255 ->
	    Vars =.. [vars|VarList]
	;   Vars = VarList
	),
	catch(validate_test_options(Name, Options0, Options),
	      E,
	      % When the test is run it will (once again) reports the problem with the options.
	      % By re-throwing the exception we will get a more descriptive text ("expected option, but found a_bad_set_option(bork)").
	      % By using blocked/1 would get a more discreet indication that something is wrong. I do not think
	      % discreet is disirable for malformed tests.
	      Options = [setup(throw(E))]
	     ).


% Validate and canonicalize test/2 options.
% xref test_option/1
validate_test_options(Name, Options1, Options) :-
	Goal = test(Name,Options1),
	ArgNo = 2,
	valid_options(Options1, Options2, test_option, Goal, ArgNo),
	(   findall(Opt, option_incompatible_with(Opt, _), Templates1),
	    sort(Templates1, Templates),
	    member(Option, Templates),
	    (   select(Option, Options2, OtherOptions) ->
		(   member(Other, OtherOptions),
		    option_incompatible_with(Option, Other)
		->
		    illarg(consistency(Option,Other,
				       'incompatible test options'),
			   Goal, ArgNo)
		)
	    ),
	    fail                % redundant
	;   true
	),
	% Combine test options, e.g. setup(A),setup(B) to setup((A,B)).
	% (multiple true/1 are not combined here, they are handled elsewhere).
	canonicalize_test_options(Options2, Options).


:- expand(+Term, +TermLay, -Clauses, -ClausesLay) is semidet.

expand((test(Name):-Body), Lay0, Clauses, Lay) :-
	!,
	expand_test(Name, [], Body, Lay0, Clauses, Lay).
expand((test(Name,Options):-Body), Lay0, Clauses, Lay) :-
	!,
	expand_test(Name, Options, Body, Lay0, Clauses, Lay).
expand(test(Name), Lay0, Clauses, Lay) :-
	!,
	NoLayout = [],
	comp_layout2(Lay0, Lay0, NoLayout, Lay1),
	expand((test(Name):-true), Lay1, Clauses, Lay).
expand(test(Name,Options), Lay0, Clauses, Lay) :-
	!,
	NoLayout = [],
	comp_layout2(Lay0, Lay0, NoLayout, Lay1),
	expand((test(Name,Options):-true), Lay1, Clauses, Lay).
% [PM] 4.3.2 end_of_file is now handled in caller instead.
%expand(end_of_file, _) :-
%	prolog_load_context(source, Source),
%	retractall(seen_unit(Source)),
%	loading_unit(Unit, _, _, _), !,
%	end_tests(Unit),		% warn?
%	fail.

user:term_expansion((:- begin_tests(Set)), Lay, Ids, Expanded, Lay2, Ids2) :-
	nonmember(plunit, Ids), !,
	user:term_expansion((:- begin_tests(Set,[])), Lay, Ids, Expanded, Lay2, Ids2).
user:term_expansion((:- begin_tests(Set,Opt)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids),
	prolog_load_context(source, Source),
	seen_unit(Source), !,
	Expanded =  [ (:- plunit:begin_tests(Set,Opt)) ].
user:term_expansion((:- begin_tests(Set,Opt)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids), !,
	prolog_load_context(source, Source),
	assertz(seen_unit(Source)),
	Expanded =  [ (:- plunit:begin_tests(Set,Opt)),
		      (:- discontiguous('unit body'/4)),
		      (:- discontiguous('unit test'/5))
		    ].
user:term_expansion((:- end_tests(Set)), _Lay, Ids, Expanded, [], [plunit|Ids]) :-
	nonmember(plunit, Ids), !,
	Expanded = ((:- plunit:end_tests(Set))).
user:term_expansion(end_of_file, _Lay0, _Ids0, _Expanded, _Lay, _Ids) :-
        prolog_load_context(source, Source),
        % [PM] 4.3.2 This ensures that we emit discontiguous directives when Source is re-loaded.
        retractall(seen_unit(Source)),
        fail.
user:term_expansion(end_of_file, _Lay0, _Ids0, _Expanded, _Lay, _Ids) :-
        % [PM] 4.3.2 the idea seems to be to catch EOF after begin_tests(Unit,...), before the matching
        % end_tests(Unit), but the way it is done here would do the wrong thing if some
        % file is loaded (or included?) between begin_tests and end_tests.
        % In either case, this code does the same as the code it replaces.
        once(loading_unit(Unit, _, _, _)),
        % warn? (premature end of file)
        end_tests(Unit),
        fail.
user:term_expansion(Term, Lay0, Ids, Expanded, Lay, [plunit|Ids]) :-
	nonmember(plunit, Ids),
	catch(enabled, error(_,_), fail),
	loading_unit(_, _, Source, _),
	prolog_load_context(source, Source),
	Excp = error(_,_),
	on_exception(Excp,
		     expand(Term, Lay0, Expanded, Lay),
		     (   Expanded = [],
			 print_message(error, Excp)
		     )),
	!.

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

/*
   The predicates below are for decomposing and composing layout, to aid in the transformation of layout.
   Predicates comp_layoutN/(N+2) receive the layout of the term before the transformation,
   the layout of the possibly transformed args, and return the new layout.
 */


% Decompose the layout of a two-argument term
:- decomp_layout2(Lay, -Left, -Rigth) is det.
decomp_layout2([_LN,L0,R0], L, R) :-
	!,
	L = L0,
	R = R0.
decomp_layout2(L, L, L).

% Build the layout of a two-argument term from the layout (-line)
% of some term and the layout of the new arguments.
:- comp_layout2(+OldLayout, +NewLeftLayout, +NewRightLayout, -Layout) is det.
comp_layout2([LN|_], L0, R0, Layout) :-
	!,
	Layout = [LN,L0,R0].
comp_layout2(LN, L0, R0, Layout) :-
	integer(LN),
	!,
	Layout = [LN,L0,R0].
comp_layout2(_, _, _, []).

source_location(File, Line) :-
	prolog_load_context(file, File),
	prolog_load_context(term_position, Pos),
	stream_position_data(line_count, Pos, Line).

		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

%%	valid_options(+Options, :Pred) is det.
%
%	Verify Options to be a list of valid options according to
%	Pred.
%
%	@throws	=type_error= or =instantiation_error=.

valid_options(Options0, Options, Pred, Goal, ArgNo) :-
	(   Options0 = [] ->    Options = []
	;   Options0 = [H|T] -> Options = [H|T]
	;   true ->             Options  = [Options0]
	),
	must_be(Options, proper_list(nonvar), Goal, ArgNo),
	(   foreach(Opt,Options),
	    param(Pred,Goal,ArgNo)
	do  (   call(Pred, Opt) -> true
	    ;   illarg(domain(term,option), Goal, ArgNo, Opt)
	    )
	).

%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for test(Name, Options).

test_option(Option) :-
	test_set_option(Option), !.
test_option(true).		% for compatibility
test_option(true(_)).
test_option(all(_)).
test_option(set(_)).
test_option(fail).
test_option(exception(_)).	% SICStus specific
test_option(throws(_)).
test_option(error(_)).
test_option(error(_,_)).	% SICStus specific
% test_option(sto(_)).		% SWI specific
test_option(nondet).
test_option(forall(X)) :-
	callable(X).

% +TestOption ==> -IncompatibleOption (for those options that are not also test_set_option/1)
% Note: Avoiods some symmetries by not containing entries where first argumet (TestOption) is true or true(_).
:- discontiguous option_incompatible_with/2.

% all/1
option_incompatible_with(all(_), Other) :- option_incompatible_with_set(Other).
% set/1
option_incompatible_with(set(_), Other) :- option_incompatible_with_set(Other).

% all/1 and set/1
option_incompatible_with_set(true).
option_incompatible_with_set(true(_)).
option_incompatible_with_set(all(_)).
option_incompatible_with_set(set(_)).
option_incompatible_with_set(fail).
option_incompatible_with_set(exception(_)).
option_incompatible_with_set(throws(_)).
option_incompatible_with_set(error(_)).
option_incompatible_with_set(error(_,_)).
option_incompatible_with_set(nondet). % Pointless
% option_incompatible_with_all(forall(_)).

% fail/0
option_incompatible_with(fail, Other) :- option_incompatible_with_fail(Other).

option_incompatible_with_fail(true).
option_incompatible_with_fail(true(_)).
option_incompatible_with_fail(all(_)).
option_incompatible_with_fail(set(_)).
option_incompatible_with_fail(fail). % Pointless
option_incompatible_with_fail(exception(_)).
option_incompatible_with_fail(throws(_)).
option_incompatible_with_fail(error(_)).
option_incompatible_with_fail(error(_,_)).
option_incompatible_with_fail(nondet). % Pointless
% option_incompatible_with_fail(forall(_)).

% exception/1 et al.
option_incompatible_with(exception(_), Other) :- option_incompatible_with_exception(Other).
option_incompatible_with(throws(_), Other) :- option_incompatible_with_exception(Other).
option_incompatible_with(error(_), Other) :- option_incompatible_with_exception(Other).
option_incompatible_with(error(_,_), Other) :- option_incompatible_with_exception(Other).

option_incompatible_with_exception(true).
option_incompatible_with_exception(true(_)).
option_incompatible_with_exception(all(_)).
option_incompatible_with_exception(set(_)).
option_incompatible_with_exception(fail).
option_incompatible_with_exception(exception(_)).
option_incompatible_with_exception(throws(_)).
option_incompatible_with_exception(error(_)).
option_incompatible_with_exception(error(_,_)).
option_incompatible_with_exception(nondet). % Pointless
% option_incompatible_with_exception(forall(_)).

% nondet/0
option_incompatible_with(nondet, Other) :-
	option_incompatible_with_nondet(Other).

% option_incompatible_with_nondet(true).
% option_incompatible_with_nondet(true(_)).
option_incompatible_with_nondet(all(_)).
option_incompatible_with_nondet(set(_)).
option_incompatible_with_nondet(fail).
option_incompatible_with_nondet(exception(_)).
option_incompatible_with_nondet(throws(_)).
option_incompatible_with_nondet(error(_)).
option_incompatible_with_nondet(error(_,_)).
option_incompatible_with_nondet(nondet).
% option_incompatible_with_nondet(forall(_)).

% forall/1
option_incompatible_with(forall(_), Other) :- option_incompatible_with_forall(Other).

% option_incompatible_with_forall(true).
% option_incompatible_with_forall(true(_)).
% option_incompatible_with_forall(all(_)).
% option_incompatible_with_forall(set(_)).
% option_incompatible_with_forall(fail).
% option_incompatible_with_forall(exception(_)).
% option_incompatible_with_forall(throws(_)).
% option_incompatible_with_forall(error(_)).
% option_incompatible_with_forall(error(_,_)).
% option_incompatible_with_forall(nondet).
option_incompatible_with_forall(forall(_)).


%%	test_option(+Option) is semidet.
%
%	True if Option is a valid option for :- begin_tests(Name,
%	Options).

test_set_option(blocked(X)) :-
	ground(X).
test_set_option(fixme(X)) :-
	ground(X).
test_set_option(condition(X)) :-
	callable(X).
test_set_option(setup(X)) :-
	callable(X).
test_set_option(cleanup(X)) :-
	callable(X).

% Replace any number of F(Arg) with a conjunct, e.g.
% [.A., F(Arg1), .B., F(Arg2), .C.] ==> [.A., F((Arg1,Arg2)), .B., .C.])).
merge_options_conjunct(Options1, F, Options) :-
	merge_options_with(Options1, F, (','), Options).

% % Replace any number of F(Arg) with a disjunct, e.g.
% % [.A., F(Arg1), .B., F(Arg2), .C.] ==> [.A., F((Arg1;Arg2)), .B., .C.])).
% merge_options_disjunct(Options1, F, Options) :-
% 	merge_options_with(Options1, F, (';'), Options).


% Replace any number of F(Arg) with a nested Joiner-pairs, e.g.
% [.A., F(Arg1), .B., F(Arg2), .C.] ==> [.A., F(Joiner(Arg1,Arg2)), .B., .C.])).
merge_options_with([], _F, _Joiner, []).
merge_options_with([Option|Options1], F, Joiner, Options) :-
	functor(Option, F, 1),
	!,
	arg(1, Option, Arg),
	merge_option_with(Options1, F, Arg, AllArgs, Joiner, Options2),
	Option1 =.. [F,AllArgs],
	Options = [Option1|Options2].
merge_options_with([Option|Options], Template, Joiner, [Option|Options1]) :-
	merge_options_with(Options, Template, Joiner, Options1).

merge_option_with([], _F, Arg, AllArgs, _Joiner, Options) :-
	AllArgs = Arg,
	Options = [].
merge_option_with([Option|Options1], F, Arg, AllArgs, Joiner, Options) :-
	functor(Option, F, 1),
	!,
	arg(1, Option, Arg1),
	Arg2 =.. [Joiner,Arg,Arg1],
	merge_option_with(Options1, F, Arg2, AllArgs, Joiner, Options).
merge_option_with([Option|Options1], F, Arg, AllArgs, Joiner, [Option|Options]) :-
	merge_option_with(Options1, F, Arg, AllArgs, Joiner, Options).




		 /*******************************
		 *	  RUNNING TOPLEVEL	*
		 *******************************/


%%	run_tests is det.
%%	run_tests(+TestSet) is det.
%%	run_tests(+TestSet, +Options) is det.
:- run_tests/0 is det.
run_tests :-
	run_tests(all, []).

:- run_tests/1 is det.
run_tests(Raw) :-
	run_tests(Raw, []).

:- run_tests/2 is det.
run_tests(Raw, Options0) :-	% Options are SICStus specific
	Goal = run_tests(Raw,Options0),
	valid_options(Options0, Options, run_option, Goal, 2),
	(   Raw == all ->
	    findall(U, current_unit(U,_,_,_), Set0)
	;   otherwise ->
	    ensure_list(Raw, Set0),
	    must_be(Set0, proper_list(nonvar), Goal, 1)
	),
	(   foreach(Elt0,Set0),
	    foreach(Elt,Set),
	    param(Goal)
	do  (   atom(Elt0)
	    ->  Elt = Elt0
	    ;   Elt0 = (Name:Tests0),
		atom(Name),
		ensure_list(Tests0, Tests),
		must_be(Tests, proper_list(nonvar), Goal, 1)
	    ->  Elt  = (Name:Tests )
	    ;   illarg(domain(callable,test_spec), Elt, Goal, 1)
	    )
	),
	cleanup,
	(   foreach(Spec,Set),
	    param(Options)
	do  unit_from_spec(Spec, Unit, Tests1, Module, UnitOptions),
	    Context = unit(Unit),
	    (   member(blocked(Reason), UnitOptions)
	    ->  print_informational(plunit(blocked(unit(Unit, Reason))), Options)
	    ;   member(fixme(Reason), UnitOptions)
	    ->  print_informational(plunit(fixme(unit(Unit, Reason))), Options)
	    ;   setup(Module, Context, UnitOptions)
	    ->  print_informational(plunit(begin(Spec)), Options),
		call_cleanup(run_unit_tests(Unit, Tests1, Module, Options),
			     cleanup_unit_tests(Spec, Options, Module, Context, UnitOptions))
	    ;   true
	    )
	),
	report(Options).

run_option(quiet).
run_option(verbose).
run_option(passed(_)).
run_option(failed(_)).
run_option(skipped(_)).

ensure_list(X, L) :-
	(   nonvar(X), X = [_|_] -> L = X ; L = [X]   ).

run_unit_tests(Unit, Tests, Module, Verbosity) :-
	\+ (   (   Module:'unit test'(Unit, Name, FileLine, Options, Body),
		   memberchk(Name, Tests),
		   append(Options, Verbosity, OptionsV)
	       ),
	       \+run_test(Unit, Name, FileLine, OptionsV, Body)
	   ).

cleanup_unit_tests(Spec, Options, Module, Context, UnitOptions) :-
	print_informational(plunit(end(Spec)), Options),
	cleanup(Module, Context, UnitOptions).

unit_from_spec(Unit, Unit, _, Module, Options) :-
	atom(Unit), !,
	(   current_unit(Unit, Module, _, Options)
	->  true
	;   illarg(existence(unit_test,Unit,0), run_tests(Unit), 1)
	).
unit_from_spec(Unit:Tests, Unit, Tests, Module, Options) :-
	atom(Unit), !,
	(   current_unit(Unit, Module, _, Options)
	->  true
	;   illarg(existence(unit_test,Unit,0), run_tests(Unit:Tests), 1)
	).

cleanup :-
	retractall(passed(_, _, _, _, _)),
	retractall(failed(_, _, _, _)),
	retractall(skipped(_, _, _)),
	retractall(blocked(_, _, _, _)),
	retractall(fixme(_, _, _, _, _)).

		 /*******************************
		 *	   RUNNING A TEST	*
		 *******************************/

%%	run_test(+Unit, +Name, +FileLine, +Options, +Body) is det.
%
%	Run a single test.

run_test(Unit, Name, FileLine, Options, _Body) :-
	member(blocked(Reason), Options), !,
	assertz(blocked(Unit, Name, FileLine, Reason)).
run_test(Unit, Name, FileLine, Options, Body) :-
	member(forall(Generator), Options), !,
	unit_module(Unit, Module),
	% Was term_variables_set/2, but surely left-to-right order is more predictable for the user?
	term_variables(Generator, Vars),
	\+ (   catch(call(Module:Generator), E, true),
	       \+ (   var(E)
		  ->  run_test_once(Unit, @(Name,Vars), FileLine, Options, Body)
		  ;   How = forall(excp(E)),
		      report_result(failure(Unit,Name,FileLine,How), Options)
		  )
	   ).
run_test(Unit, Name, FileLine, Options, Body) :-
	run_test_once(Unit, Name, FileLine, Options, Body).


run_test_once(Unit, Name, FileLine, Options, Body) :-
	unit_module(Unit, Module),
	Context = test(Unit,Name,FileLine),
	setup(Module, Options, ConditionsResult, SetupResult),
	(   ConditionsResult = succeeded ->
	    (   SetupResult = succeeded ->
		call_cleanup(call_test(Module, Body, Options, Context, Result),
			     cleanup(Module, Context, Options)),
		report_result(Result, Options)
	    ;   SetupResult = excp(SE) ->
		How = setup(excp(SE)),
		report_result(failure(Unit,Name,FileLine,How), Options)
	    ;   SetupResult = failed(Setup) ->
		How = setup(failed(Setup)),
		report_result(failure(Unit,Name,FileLine,How), Options)
	    )
	;   ConditionsResult = failed(Setup) ->
	    How = condition(failed(Setup)),
	    report_result(failure(Unit,Name,FileLine,How), Options)
	;   ConditionsResult = excp(CE) ->
	    How = condition(excp(CE)),
	    report_result(failure(Unit,Name,FileLine,How), Options)
	).

report_result(failure(Unit,Name,FileLine,How), Options) :-
	(   How = condition(failed(_)) % quietly ignored, but counted
	->
	    assertz(skipped(Unit, Name, FileLine))
	;   member(fixme(Reason), Options)
	->
	    assertz(fixme(Unit, Name, FileLine, Reason, failed))
	;   otherwise
	->
	    Context = test(Unit,Name,FileLine),
	    % [PM] 4.5. We use the same message formats as in pre-4.5 for problems with condition/1 or setup/1.
	    (   How = condition(excp(CE)) ->
		print_message(error, plunit(error(condition,Context,CE)))
	    ;   How = setup(excp(SE)) ->
		print_message(error, plunit(error(setup,Context,SE)))
	    ;   How = setup(failed(Setup))
	    ->
		print_message(error, plunit(error(setup,Context,error(_,failed(Setup)))))
	    ;   How = forall(excp(FE))
	    ->
		print_message(error, plunit(error(forall,test(Unit,Name,FileLine),FE)))
	    ;
		print_message(error, plunit(failed(Unit,Name,FileLine,How)))
	    ),
	    assertz(failed(Unit, Name, FileLine, How))
	).
report_result(success(Unit, Name, FileLine, Det, Time), Options) :-
	Ok = passed,            % Always passed, since we no longer get here if unexpected nondet
	(   member(fixme(Reason), Options)
	->  assertz(fixme(Unit, Name, FileLine, Reason, Ok))
	;   assertz(passed(Unit, Name, FileLine, Det, Time)),
	    print_informational(plunit(succeeded(Unit, Name, FileLine, Det, Time)), Options)
	).

:- call_test/5 is nondet.
call_test(Module, Body0, Options, Context, Result) :-
	get_options(Options, Body0, Body, ExitOpt),
	statistics(runtime, [T0,_]),
	(   catch(call_det(Module:Body, Det), E, true)
	->  (   var(E)
	    ->  report_result(true(Det), T0, Context, Module, ExitOpt, Result)
	    ;   report_result(excp(E),   T0, Context, Module, ExitOpt, Result)
	    )
	;   report_result(fail, T0, Context, Module, ExitOpt, Result)
	).

%%	call_det(:Goal, -Det) is nondet.
%
%	True if Goal succeeded.  Det is unified to =true= if Goal left
%	no choicepoints and =false= otherwise.

:- call_det/2 is nondet.
call_det(Goal, Det) :-
	call_cleanup(Goal, Det0=true),
	( var(Det0) -> Det = nondet ; Det = det ).

report_result(GotExit, T0, test(Unit,Name,FileLine), Module, ExpExit, Result) :-
	statistics(runtime, [T1,_]),
	Time is (T1 - T0)/1000.0,
	report_result(GotExit, ExpExit, Time, Module, Unit, Name, FileLine, Result).

report_result(true(Det), true(Cmps,ExpDet), Time, Module, Unit, Name, FileLine, Result) :-
	!,
	evaluate_result(Cmps, Module, Unit, Name, FileLine, success(Unit,Name,FileLine,Det,Time), Result1),
	(   Result1 = success(Unit,Name,FileLine,Det,Time),
	    Det == nondet,
	    ExpDet == det                            % Unexpected 'nondet; is treated as failure but unexpected 'det' is permitted
	->
	    Result = failure(Unit,Name,FileLine,Det) % success but with unaccepable (non-)determinism
	;
	    Result = Result1                         % success with acceptable determinism, or some failure
	).

report_result(true(_Det),  fail      , Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, succeeded(Time)).
report_result(true(_Det),  excp(_ExpE), _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, no_exception).
report_result(fail,       true(_Cmps,_ExpDet) , _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, failed).
report_result(fail,       fail      , Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = success(Unit, Name, FileLine, det, Time).
report_result(fail,       excp(_ExpE), _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, failed).
report_result(excp(GotE), true(_Cmps,_ExpDet),  _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, excp(GotE)).
report_result(excp(GotE), fail     ,  _Time, _Module, Unit, Name, FileLine, Result) :- !,
	Result = failure(Unit, Name, FileLine, excp(GotE)).
report_result(excp(GotE), excp(ExpE), Time, _Module, Unit, Name, FileLine, Result) :- !,
	(   subsumes_term(ExpE,GotE)
	->  Result = success(Unit, Name, FileLine, det, Time)
	;   Result = failure(Unit, Name, FileLine, wrong_exception(ExpE,GotE))
	).

:- evaluate_result/7 is nondet.
evaluate_result([], _Module, _Unit, _Name, _FileLine, Result1, Result) :-
	Result = Result1.
evaluate_result([Cmp|Cmps],  Module, Unit, Name, FileLine, Result1, Result) :-
	(   catch(call(Module:Cmp), E, true)
	->
	    (   var(E)
	    ->
		evaluate_result(Cmps, Module, Unit, Name, FileLine, Result1, Result)
	    ;   print_message(warning, plunit(error(true,test(Unit,Name,FileLine),E))),
		Result = failure(Unit,Name,FileLine,error)
	    )
	;   Result = failure(Unit,Name,FileLine,wrong_answer(Cmp))
	).


% Binds Exit to a term of the form true(Cmps, ExpDet), fail, or excp(T)
% Where Cmps is a, possibly empty, list of goals
% Where ExpDet is 'det' (default) or 'nondet' (corresponding to the nondet option).
get_options(Options, Body0, Body, Exit) :-
	get_options_(Options, Body0, Body, Exit1, Conj, Det),
	(   var(Exit1) ->
	    Exit = true(Conj,Det)
	;   Exit1 = true(Cond) ->
	    Exit = true([Cond],Det)
	;   Exit = Exit1
	).


% Binds Exit to a term of the _Var, true(Cmp), fail, or excp(T).
% Binds Conj to a list of goals that should be true (should be empty if Exit is nonvar).
get_options_(Options, Body0, Body, Exit, Conj, Det) :-
	(   foreach(Opt,Options),
	    fromto(_None, Exit1,Exit2,Exit),
	    fromto(Conj,  Conj1,Conj2,[]),
	    fromto(Body0, Body1,Body2,Body),
	    fromto(det,   Det1,Det2,  Det)
	do  (   Opt = true
	    ->  Exit2 = Exit1,
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = true(T)
	    ->  Exit2 = Exit1,
		Body2 = Body1,
		Conj1 = [T|Conj2],
		Det2 = Det1
	    ;   Opt = nondet
	    ->  Exit2 = Exit1,
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = nondet
	    ;   Opt = fail
	    ->  Exit2 = fail,
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = exception(T)
	    ->  Exit2 = excp(T),
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = throws(T)
	    ->  Exit2 = excp(T),
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = error(T)
	    ->  Exit2 = excp(error(T,_)),
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = error(T,U)
	    ->  Exit2 = excp(error(T,U)),
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = all(Cmp1)
	    ->  Cmp1 =.. [F,X|Ys],
		Cmp2 =.. [F,Xs|Ys],
		Exit2 = true(Cmp2),
		Body2 = findall(X,Body1,Xs),
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Opt = set(Cmp1)
	    ->  Cmp1 =.. [F,X|Ys],
		Cmp2 =.. [F,Xs|Ys],
		Exit2 = true(Cmp2),
		Body2 = (findall(X,Body1,Xs0), sort(Xs0,Xs)),
		Conj1 = Conj2,
		Det2 = Det1
	    ;   Exit2 = Exit1,
		Body2 = Body1,
		Conj1 = Conj2,
		Det2 = Det1
	    )
	).



%% setup(+Module, +Context, +Options) is semidet.
%
% Call the setup handler and  fail  if   it  cannot  run  for some
% reason. The condition handler is  similar,   but  failing is not
% considered an error.  Context is one of
%
% * unit(Unit)
% If it is the setup handler for a unit
% * test(Unit,Name,FileLine)
% If it is the setup handler for a test
setup(Module, Context, Options) :-
	setup(Module, Options, ConditionsResult, SetupResult),
	(   ConditionsResult = succeeded ->
	    true
	;   ConditionsResult = excp(CE) ->
	    print_message(error, plunit(error(condition,Context,CE))),
	    fail
	;   ConditionsResult = failed(_) ->
	    fail
	),
	(   SetupResult = succeeded ->
	    true
	;   SetupResult = excp(SE) ->
	    print_message(error, plunit(error(setup,Context,SE))),
	    fail
	;   SetupResult = failed(Setup) ->
	    print_message(error, plunit(error(setup,Context,error(_,failed(Setup))))), % hack alert
	    fail
	).



% Run condition and (if condition succeeds) the setup.
% Always succeeds and returns the result of any condition and setup goals.
% Does _not_ print any messages.
% Results are one of failed(Goal), excp(Exception), or either 'succeeded' or a variable (meaning goal succeeded, goal was absent, or not run).
setup(Module, Options, ConditionsResult, SetupResult) :-
	member(condition(Condition), Options),
	member(setup(Setup), Options),
	!,
	setup(Module, [condition(Condition)], ConditionsResult1, _),
	(   ConditionsResult1 = succeeded ->
	    setup(Module, [setup(Setup)], _, SetupResult1)
	;   otherwise ->
	    SetupResult1 = _
	),
	ConditionsResult = ConditionsResult1,
	SetupResult = SetupResult1.
setup(Module, Options, _ConditionsResult, SetupResult) :-
	member(setup(Setup), Options),
	!,
	run_setup_goal(Setup, Module, SetupResult).
setup(Module, Options, ConditionsResult, _SetupResult) :-
	member(condition(Setup), Options),
	!,
	run_setup_goal(Setup, Module, ConditionsResult).
setup(_, _, _ConditionsResult, _SetupResult).


run_setup_goal(Setup, Module, ConditionsResult) :-
	(   catch(call(Module:Setup), E, true)
	->
	    (   var(E) ->
		ConditionsResult = succeeded
	    ;   ConditionsResult = excp(E)
	    )
	;   ConditionsResult = failed(Setup)
	).

%%	cleanup(+Module, +Context, +Options) is det.
%
%	Call the cleanup handler and succeed.	Failure	 or error of the
%	cleanup handler is reported, but tests continue normally.

cleanup(Module, Context, Options) :-
	member(cleanup(Cleanup), Options), !,
	(   catch(call(Module:Cleanup), E, true) -> true
	;   E = error(_,failed(Cleanup))
	),
	(   var(E) -> true
	;   print_message(warning, plunit(error(cleanup, Context, E)))
	).
cleanup(_, _, _).

		 /*******************************
		 *	      REPORTING		*
		 *******************************/

%%	report is semidet.
%
%	True if there are no errors.  If errors were encountered, report
%	them to current output and fail.

report(Options) :-
	number_of_clauses(passed/5, Passed),
	number_of_clauses(failed/4, Failed),
	number_of_clauses(skipped/3, Skipped),
	print_informational(plunit(passed(Passed)), Options),
	print_informational(plunit(failed(Failed)), Options),
	print_informational(plunit(skipped(Skipped)), Options),
	report_blocked(Options),
	report_fixme(TuplesF, TuplesP, TuplesN, Options),
	append(TuplesP, TuplesN, TuplesPN),
	append(TuplesF, TuplesPN, Tuples),
	print_informational(plunit(fixme(Tuples)), Options),
	% Bind output variables after all reporting is done, since it can fail.
	(   member(passed(Passed1), Options) ->
	    Passed1 = Passed
	;   true
	),
	(   member(failed(Failed1), Options) ->
	    Failed1 = Failed
	;   true
	),
	(   member(skipped(Skipped1), Options) ->
	    Skipped1 = Skipped
	;   true
	),
	true.

number_of_clauses(F/A, N) :-
	(   current_predicate(F/A)
	->  functor(G, F, A),
	    findall(t, G, Ts),
	    length(Ts, N)
	;   N = 0
	).

report_blocked(Options) :-
	number_of_clauses(blocked/4, N),
	N > 0, !,
	print_informational(plunit(blocked(N)), Options),
	(   blocked(_, Name, FileLine, Reason),
	    print_informational(plunit(blocked(FileLine, Name, Reason)), Options),
	    fail
	;   true
	).
report_blocked(_).

report_fixme(TuplesF, TuplesP, TuplesN, Options) :-
	fixme(failed, TuplesF, Failed),
	fixme(passed, TuplesP, Passed),
	fixme(nondet, TuplesN, Nondet),
	print_informational(plunit(fixme(Failed, Passed, Nondet)), Options).

print_informational(_Term, Options) :-
	member(quiet, Options), !.
print_informational(Term, _) :-
	print_message(informational, Term).


fixme(How, Tuples, Count) :-
	findall(fixme(Unit, Name, FileLine, Reason, How),
		fixme(Unit, Name, FileLine, Reason, How), Tuples),
	length(Tuples, Count).

		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

locationprefix(File:Line) --> !,
	[ '~w:~d:'-[File,Line],nl,'\t'-[]].
locationprefix(test(_,_,FileLine)) --> !,
	locationprefix(FileLine).
locationprefix(unit(Unit)) --> !,
	[ 'PL-Unit: unit ~w: '-[Unit],nl,'\t'-[] ].
locationprefix(FileLine) -->
	{ illarg(domain(term,locationprefix), 0, 0, FileLine) }.

message(error(context_error(plunit_close(Name, -)), _)) --> !,
	[ 'PL-Unit: cannot close unit ~w: no open unit'-[Name],nl ].
message(error(context_error(plunit_close(Name, Start)), _)) -->
	[ 'PL-Unit: cannot close unit ~w: current unit is ~w'-[Name, Start],nl ].

					% Unit start/end
message(plunit(begin(Unit))) --> !,
	[ 'PL-Unit: ~w '-[Unit],nl ].
message(plunit(end(_Unit))) --> !,
	[ 'done'-[],nl ].
message(plunit(blocked(unit(Unit, Reason)))) --> !,
	[ 'PL-Unit: ~w blocked: ~w'-[Unit,Reason],nl ].
message(plunit(fixme(unit(Unit, Reason)))) --> !,
	[ 'PL-Unit: ~w fixme: ~w'-[Unit,Reason],nl ].
message(plunit(fixme(Tuples))) --> !,
	(   foreach(fixme(_Unit, _Name, FileLine, Reason, How),Tuples)
	do  fixme_message(FileLine, Reason, How)
	).
					% Blocked tests
message(plunit(blocked(1))) --> !,
	[ 'one test is blocked:'-[],nl ].
message(plunit(blocked(N))) --> !,
	[ '~D tests are blocked:'-[N],nl ].
message(plunit(blocked(Pos, Name, Reason))) --> !,
	locationprefix(Pos),
	test_name(Name),
	[ '~w'-[Reason],nl ].
					% fail/success
message(plunit(no_tests)) --> !,
	[ 'No tests to run'-[],nl ].
message(plunit(all_passed(Count))) --> !,
	[ 'All ~D tests passed'-[Count],nl ].
message(plunit(skipped(0))) --> !,
	[].
message(plunit(skipped(1))) --> !,
	[ '1 test skipped'-[],nl ].
message(plunit(skipped(N))) --> !,
	[ '~D tests skipped'-[N],nl ].
message(plunit(failed(0))) --> !,
	[].
message(plunit(failed(1))) --> !,
	[ '1 test failed'-[],nl ].
message(plunit(failed(N))) --> !,
	[ '~D tests failed'-[N],nl ].
message(plunit(passed(N))) --> !,
	[ '~D tests passed'-[N],nl ].
message(plunit(fixme(0,0,0))) --> !,
	[].
message(plunit(fixme(Failed,0,0))) --> !,
	[ 'all ~D tests flagged FIXME failed'-[Failed],nl ].
message(plunit(fixme(Failed,Passed,0))) --> !,
	[ 'FIXME: ~D failed; ~D passed'-[Failed, Passed],nl ].
message(plunit(fixme(Failed,Passed,Nondet))) --> !,
	{ TotalPassed is Passed+Nondet },
	[ 'FIXME: ~D failed; ~D passed; (~D nondet)'-[Failed, TotalPassed, Nondet],nl ].
message(plunit(failed(_Unit, Name, FileLine, Failure))) --> !,
       locationprefix(FileLine),
       test_name(Name),
       failure(Failure).
message(plunit(succeeded(_Unit, Name, FileLine, Det, Time))) --> !,
       locationprefix(FileLine),
       test_name(Name),
       [ 'succeeded (~w) in ~2f seconds'-[Det,Time],nl ].
					% Setup/condition errors
message(plunit(error(Where, Context, Exception))) --> !,
	locationprefix(Context),
	test_name(Context),
	[ 'error in ~w'-[Where],nl ],
	error(Exception).

test_name(@(Name,Bindings)) --> !,
	[ 'test ~w (forall bindings = ~p): '-[Name, Bindings] ].
test_name(unit(_)) --> !.
test_name(test(_,Name,_)) --> !,
	test_name(Name).
test_name(Name) --> !,
	[ 'test ~w: '-[Name] ].

expected_got_ops_(Ex, E, Goals) -->
	['    Expected: ~q'-[Ex],nl],
	['    Got:      ~q'-[E ],nl],
	( { Goals = true } -> []
	; ['       with: ~q'-[Goals],nl]
	).


failure(Var) -->
	{ var(Var) }, !,
	[ 'Unknown failure?'-[],nl ].
failure(no_exception) --> !,
	[ 'should raise exception but did not'-[],nl ].
failure(succeeded(Time)) --> !,
	[ 'should fail but succeeded in ~2f seconds'-[Time],nl ].
failure(nondet) --> !,
	[ 'succeeded nondet'-[],nl ].
failure(wrong_exception(Expected, Error)) --> !,
	{ copy_term(Expected-Error, Ex-E, Goals),
	  numbervars(Ex-E-Goals, 0, _)
	},
	[ 'wrong exception'-[], nl ],
	expected_got_ops_(Ex, E, Goals).
failure(wrong_answer(Cmp)) -->
	{ Cmp =.. [Op,Answer,Expected], !,
	  copy_term(Expected-Answer, Ex-A, Goals),
	  numbervars(Ex-A-Goals, 0, _)
	},
	[ 'wrong answer (compared using ~w)'-[Op], nl ],
	expected_got_ops_(Ex, A, Goals).
failure(wrong_answer(Test)) --> !,
	[ 'wrong answer: ~q - failed'-[Test], nl ].
failure(excp(error(ISO,ClassicError))) --> !,
	['raised an error exception'-[],nl],
	error(error(ISO,ClassicError)).
failure(excp(E)) --> !,
	['raised an exception'-[],nl],
	error(E).
failure(Why) -->
	[ '~q'-[Why],nl ].

error(error(_,ClassicError)) --> !,
	'SU_messages':generate_message(ClassicError), !.
error(Why) -->
	[ '~q'-[Why],nl ].

fixme_message(Location, Reason, failed) --> !,
	[ 'FIXME: ~w: ~w'-[Location, Reason],nl ].
fixme_message(Location, Reason, passed) --> !,
	[ 'FIXME: ~w: passed ~w'-[Location, Reason],nl ].
fixme_message(Location, Reason, nondet) --> !,
	[ 'FIXME: ~w: passed (nondet) ~w'-[Location, Reason],nl ].

user:generate_message_hook(Message) -->
	{catch(enabled, _, fail)},
	message(Message), !.

enabled.
