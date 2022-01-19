/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(simple, [main/0,
                   connected/4]).

:- use_module(library(jasper)).

connected(Through, To, Way, Been) :-
        connected1(Through, To, Way, Been).

connected1(From, From, [From], _) :-
        !.
connected1(From, To, [From|Way], Been) :-
        (   no_stop(From, Through)
        ;
            no_stop(Through, From)
        ),
        not_been_before(Been, Through),
        connected1(Through, To, Way, Been).

:- no_stop/2 is nondet.
no_stop('Stockholm', 'Katrineholm').
no_stop('Stockholm', 'Västerås').
no_stop('Katrineholm', 'Hallsberg').
no_stop('Katrineholm', 'Linköping').
no_stop('Hallsberg', 'Kumla').
no_stop('Hallsberg', 'Göteborg').
no_stop('Örebro', 'Västerås').
no_stop('Örebro', 'Kumla').

not_been_before(Way, _) :-
        var(Way),
        !.
not_been_before([Been|Way], Am) :-
        Been \== Am,
        not_been_before(Way, Am).

%
% main/0: does some sample calls to Java.
%
% - The 'train' predicate calls Java which in turn calls Prolog back.
% Kind of overkill, but demonstrates the general idea of callbacks to
% Prolog.
%
% "Looking? Found someone, you have, I would say, hmmm?"
main :-
        jasper_initialize([classpath([library('jasper/examples')])], JVM),
        test_strings(JVM),
        test_train(JVM),
        test_misc(JVM).

test_strings(JVM) :-
        format('~nTesting string manipulations...~n------------------------------------~n~n', []),
        jasper_new_object(JVM,
			  'Simple',
			  init(+string,+integer),
			  init('String-object',1),
			  Obj1),
	%% We can use the foreign declaration to make a method call via
	%% the meta call interface
        call_foreign_meta(JVM, get_str(Obj1,S0)),
        format('Created string: ~w~n', [S0]),
        jasper_new_object(JVM,
			  'Simple',
			  init(+string,+integer),
			  init('String-object',2),
			  Obj2),
	%% Explicit meta call
        jasper_call(JVM,
		    method('',get,[instance]),
		    get(+object('Simple'),[-string]),
		    get(Obj2,S1)),
        format('Created string: ~w~n', [S1]),
        format('Modifying string 1 using foreign/2-declared methods via meta call...~n', []),
        call_foreign_meta(JVM, set_str(Obj1,'Looking? ')),
        call_foreign_meta(JVM, append_str(Obj1,'Found someone, ')),
        call_foreign_meta(JVM, append_str(Obj1,'you have, ')),
        call_foreign_meta(JVM, get_str(Obj1,S2)),
        format('String is now: ~w~n', [S2]),
        format('Modifying string 2 using meta calls...~n', []),
	%% Make explicit meta calls
        jasper_call(JVM,
		    method('',set,[instance]),
		    set(+object('Simple'),+string),
		    set(Obj2,'you have, ')),
        jasper_call(JVM,
		    method('',append,[instance]),
		    append(+object('Simple'),+string),
		    append(Obj2,'I would say, ')),
        jasper_call(JVM,
		    method('',append,[instance]),
		    append(+object('Simple'),+string),
		    append(Obj2,'hmmm? ')),
        jasper_call(JVM,
		    method('',get,[instance]),
		    get(+object('Simple'),[-string]),
		    get(Obj2,S3)),
        format('String is now: ~w~n', [S3]),
        format('Concatenating the two strings...~n', []),
        call_foreign_meta(JVM, append_str(Obj1,S3)),
        call_foreign_meta(JVM, get_str(Obj1,S4)),
        format('Final string is: ~w~n', [S4]).

test_train(JVM) :-
        format('~nTesting train program...~n------------------------------------~n~n', []),
        format('Calling Java to do callback to Prolog...~n', []),
        call_foreign_meta(JVM, train(X)),
        format('Expected ~q~n', [['Stockholm','Katrineholm','Hallsberg','Kumla','Örebro']]),
        format('Result = ~q~n', [X]).

test_misc(JVM) :-
        format('~nTesting misc stuff...~n------------------------------------~n~n', []),
        jasper_call(JVM,
		    method('Simple',square,[static]),
		    square(+integer,[-integer]),
		    square(395,S1)),
        format('(metacall) square(395) = ~w~n', [S1]).


%%% Make a method call via the meta call interface.
%%% This is how to easily convert your code from using foreign
%%% resources to using meta calls. Meta calls are not dependent on
%%% foreign resources, so all you have to do is keep the foreign
%%% declarations, remove the foreign_resource declarations and remove
%%% the load_foreign_resource directive.
call_foreign_meta(JVM, Goal) :-
        functor(Goal, Name, Arity),     % extract predicate name
        functor(ArgDesc, Name, Arity),  % build template
        foreign(Method, java, ArgDesc), % look it up
        !,
        % format(user_error, "~N Calling Meta Call (~w) ~q~n", [Method, Goal]),
        jasper_call(JVM, Method, ArgDesc, Goal).
:- if(current_prolog_flag(dialect, spider)).
% SPIDER will complain about the foreign declarations, so hide
%% them and add an empty dummy
:- dynamic foreign/3.
:- volatile foreign/3.          % suppress optional SPIDER warning

:- else.
%% SICStus sees this
foreign(method('Simple','square',[static]), java, square(+integer, [-integer])).
foreign(method('Simple','get',[instance]), java, get_str(+object('Simple'), [-string])).
foreign(method('Simple','set',[instance]), java, set_str(+object('Simple'), +string)).
foreign(method('Simple','append',[instance]), java, append_str(+object('Simple'), +string)).
foreign(method('Simple','train',[static]), java, train([-term])).
:- endif.
