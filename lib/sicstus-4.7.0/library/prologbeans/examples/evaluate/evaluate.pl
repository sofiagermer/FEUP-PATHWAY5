/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */

:- module(evaluate, [main/0,main/1,my_predicate/2]).
:- use_module(library(prologbeans)).
:- use_module(library(codesio), [read_from_codes/2]).

%% Register acceptable queries and start the server (using default port)
main :-
	main(gui).

main(gui) :-
	register_query(evaluate(C,P), my_predicate(C, P)),
	register_query(shutdown, shutdown_server),
	start.

main(batch) :-
	register_query(evaluate(C,P), my_predicate(C, P)),
	register_query(shutdown, shutdown_server),
	register_event_listener(server_started, server_started_listener),
	register_event_listener(server_shutdown, server_shutdown_listener),
	start,
	halt.

%% In this case we know that we have received a list of characters
%% that needs to be converted into an expression.
my_predicate(Chars, P) :-
	read_from_codes(Chars, X),
	doit(X, P).


doit(X, _P) :-
	var(X),
	!,
	fail.
doit(quit, quitting) :-
	shutdown_server.
doit(Expression, P) :-
	P is Expression.

% This will be called if we build a run-time system
user:runtime_entry(start) :-
	main(batch).

server_started_listener :-
	get_server_property(port(Port)),
	format(user_error, 'port:~w~n', [Port]),
	flush_output(user_error).

server_shutdown_listener :-
	format(user_error, '~Nevaluate.pl: Shutdown~n', []),
	flush_output(user_error).

shutdown_server :-
	shutdown(now).
