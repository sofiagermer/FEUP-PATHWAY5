/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
% Copyright (C) 2020, RISE Research Institutes of Sweden AB.

/*-
  Example transcript (macOS/Linux but it is similar on Windows):

     $ sicstus --nologo --noinfo --goal 'jsonrpc_client_main,halt.' -l '$SP_LIBRARY_DIR/jsonrpc/clients/jsonrpc_client.pl'
     state ==> State is @(null)
     state:=4 ==> State was @(null)
     state ==> State is 4

     once('Result is StateIn+1, StateOut=Result.'). ==> Result=5
     once('Result is StateIn+1, StateOut=Result.'). ==> Result=6

     Increment=5, once('Result is StateIn+Increment, StateOut=Result.'). ==> Result=11

     Multiplier=10, call('member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.'). ==> First Result=111
     retry ==> (next) Result=211
     cut ==> Result=@(null)

     Multiplier=10, call('member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.'). ==> First Result=111
     retry ==> (next) Result=211
     retry ==> (next) Result=311
     retry ==> Prolog failed (this is expected)

     once('foo is bar.'). ==> Prolog threw an exception (this is expected): Exception 'type_error(evaluable,bar/0)'

     quit ==> Result='Bye'
     $ 
*/


:- module(json_client, [jsonrpc_client_main/0,
                        jsonrpc_client_main/2,
                        jsonrpc_client_logging_on/0,
                        jsonrpc_client_logging_off/0]).


:- use_module(library(json), [json_write/2,json_read/2]).

:- use_module(library(process), [process_create/3]).
:- use_module(library(lists), [append/2]).

:- meta_predicate log(?, :).
:- meta_predicate log_call(0).


% The JSON-RPC support belongs in its own module
%% JSON-RPC 2.0 support BEGIN
:- public jsonrpc_request/4.
:- public jsonrpc_request/3.

:- public jsonrpc_notification/3.
:- public jsonrpc_notification/2.

:- public jsonrpc_response/3.


:- public jsonrpc_error_response/3.
:- public jsonrpc_error_response/2.

:- public jsonrpc_error/4.
:- public jsonrpc_error/3.


% Create a JSON-RCP Request object (http://www.jsonrpc.org/specification#request_object)
jsonrpc_request(Method, Params, Id, json([jsonrpc='2.0',id=Id,method=Method,params=Params])).

jsonrpc_request(Method, Id, json([jsonrpc='2.0',id=Id,method=Method])).


% Create a JSON-RCP Notification object (http://www.jsonrpc.org/specification#notification)
jsonrpc_notification(Method, Params, json([jsonrpc='2.0',method=Method,params=Params])).

jsonrpc_notification(Method, json([jsonrpc='2.0',method=Method])).


% Create a JSON-RCP success Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_response(Result, Id, json([jsonrpc='2.0',id=Id,result=Result])).

% Create a JSON-RCP error Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_error_response(Error, Id, json([jsonrpc='2.0',id=Id,error=Error])).

% Create a JSON-RCP error Response object (http://www.jsonrpc.org/specification#response_object)
jsonrpc_error_response(Error, json([jsonrpc='2.0',id= @(null),error=Error])).


% Create a JSON-RPC Error object (http://www.jsonrpc.org/specification#error_object)
jsonrpc_error(Code, Message, Data, json([code=Code,message=Message,data=Data])).

% Create a JSON-RPC Error object (http://www.jsonrpc.org/specification#error_object)
jsonrpc_error(Code, Message, json([code=Code,message=Message])).

%% JSON-RPC 2.0 support END
% If Object is a JSON object, with a member named Name, then bind Value to the corresponding value. Otherwise,
% e.g. if there is no such member or Object is not an object, bind Value to Default.
json_member(Object, Name, Value, _Default) :-
        nonvar(Object),
        Object = json(Members),
        memberchk(Name=V, Members),
        !,
        Value = V.
json_member(_Object, _Name, Value, Default) :-
        Value = Default.


% If Object is a JSON object, with a member named Name, then bind Value to the corresponding value. Otherwise, fail.
json_member(Object, Name, Value) :-
        nonvar(Object),
        Object = json(Members),
        memberchk(Name=V, Members),
        !,
        Value = V.

jsonrpc_parse_reply(Reply, Parsed) :-
        jsonrpc_parse_reply1(Reply, Parsed1),
        !,
        Parsed = Parsed1.
jsonrpc_parse_reply(Reply, Parsed) :-
        Parsed = unknown(Reply).


jsonrpc_parse_reply1(Reply, Parsed) :-
        json_member(Reply, 'error', Error),
        !,
        json_member(Error, 'code', Code),
        (   Code = -4711 ->
            Parsed = failed
        ;   otherwise ->
            json_member(Error, 'message', Message),
            json_member(Error, 'data', Data, @(null)),
            (   Code = -4712 ->
                Parsed = exception(Message,Data)
            ;   Parsed = error(Message,Data)
            )
        ).
jsonrpc_parse_reply1(Reply, Parsed) :-
        json_member(Reply, 'result', Result),
        !,
        Parsed = success(Result).


jsonrpc_call(Method, Id, Parsed, ProcessIn, ProcessOut) :-
        jsonrpc_request(Method, Id, Request),
        json_call(Request, Reply, ProcessIn, ProcessOut),
        jsonrpc_parse_reply(Reply, Parsed).


jsonrpc_call(Method, Params, Id, Parsed, ProcessIn, ProcessOut) :-
        jsonrpc_request(Method, Params, Id, Request),
        json_call(Request, Reply, ProcessIn, ProcessOut),
        jsonrpc_parse_reply(Reply, Parsed).


json_call(Request, Reply, ProcessIn, ProcessOut) :-
        json_send(ProcessIn, Request),
        json_receive(ProcessOut, Reply).


json_receive(Stream, Reply) :-
        json_read(Stream, Reply),
        log('got back: ~q', [Reply]).


json_send(Stream, Request) :-
        log('Sending ~q', [Request]),
        json_write(Stream, Request),
        nl(Stream),
        flush_output(Stream).


:- jsonrpc_client_main/0 is det.

jsonrpc_client_main :-
        current_prolog_flag(argv, Argv),
        (   Argv = [ExePath|_] ->
            true
        ;   ExePath = '$SP_APP_PATH' % the SICStus that is running the client.
        ),
        (   Argv = [_,PlPath|_] ->
            true
        ;   PlPath = '$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server'
        ),
        jsonrpc_client_main(ExePath, PlPath).


jsonrpc_client_main(ExePath, PlPath) :-
        Load_Args = ['-l',PlPath],
        Goal_Args = ['--goal','asserta((my_call(G) :- call(G))), jsonrpc_server_main([call_hook(my_call)]),halt.'],
        (   logging ->
            Logging_Args = []
        ;   Logging_Args = ['--noinfo','--nologo']
        ),
        append([Load_Args,Goal_Args,Logging_Args], Args),
        process_create(ExePath, Args, [stdin(pipe(ProcessIn,[encoding(utf8)])),stdout(pipe(ProcessOut,[encoding(utf8)]))]),
        interact(ProcessIn, ProcessOut).


interact(ProcessIn, ProcessOut) :-
        Id = 42,                % We use the same request id, for simplicitly
        %
        % Read the initial state (it is null/Node).
        (   format(user_output, 'state ==> ', []),
            jsonrpc_call('state', Id, Parsed1, ProcessIn, ProcessOut),
            (   Parsed1 = success(Result1) ->
                format(user_output, 'State is ~q~n', [Result1])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed1])
            )
        ),
        % Set the state (to 4) and return its previous value
        (   format(user_output, 'state:=4 ==> ', []),
            jsonrpc_call('state', [4], Id, Parsed2, ProcessIn, ProcessOut),
            (   Parsed2 = success(Result2) ->
                format(user_output, 'State was ~q~n', [Result2])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed2])
            )
        ),
        % Read the state (it is 4).
        (   format(user_output, 'state ==> ', []),
            jsonrpc_call('state', Id, Parsed3, ProcessIn, ProcessOut),
            (   Parsed3 = success(Result3) ->
                format(user_output, 'State is ~q~n', [Result3])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed4])
            )
        ),
        nl(user_output),
        % Increment current state by 1.
        (   Goal1 = 'Result is StateIn+1, StateOut=Result.',
            format(user_output, 'once(~q). ==> ', [Goal1]),
            jsonrpc_call('once', json([goal=Goal1]), Id, Parsed4, ProcessIn, ProcessOut),
            (   Parsed4 = success(Result4) ->
                format(user_output, 'Result=~q~n', [Result4])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed4])
            )
        ),
        % Increment current state by 1 (again)
        (   Goal2 = 'Result is StateIn+1, StateOut=Result.',
            format(user_output, 'once(~q). ==> ', [Goal2]),
            jsonrpc_call('once', json([goal=Goal2]), Id, Parsed5, ProcessIn, ProcessOut),
            (   Parsed5 = success(Result5) ->
                format(user_output, 'Result=~q~n', [Result5])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed5])
            )
        ),
        nl(user_output),
        % Increment the state with an increment specified as a VariableName:Value pair
        (   Goal3 = 'Result is StateIn+Increment, StateOut=Result.',
            format(user_output, 'Increment=5, once(~q). ==> ', [Goal3]),
            jsonrpc_call('once', json([goal=Goal3,bindings=json(['Increment'=5])]), Id, Parsed6, ProcessIn, ProcessOut),
            (   Parsed6 = success(Result6) ->
                format(user_output, 'Result=~q~n', [Result6])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed6])
            )
        ),
        nl(user_output),
        % Call member(...), backtracking over solutions
        % Increment the state with an increment specified as a VariableName:Value pair
        (   Goal4 = 'member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.',
            format(user_output, 'Multiplier=10, call(~q). ==> ', [Goal4]),
            jsonrpc_call('call', json([goal=Goal4,bindings=json(['Multiplier'=10])]), Id, Parsed7, ProcessIn, ProcessOut),
            (   Parsed7 = success(Result7) ->
                format(user_output, 'First Result=~q~n', [Result7])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed7])
            )
        ),
        % Ask for the next solution
        (   format(user_output, 'retry ==> ', []),
            jsonrpc_call('retry', Id, Parsed8, ProcessIn, ProcessOut),
            (   Parsed8 = success(Result8) ->
                format(user_output, '(next) Result=~q~n', [Result8])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed8])
            )
        ),
        % Cut, committing to the last solution
        (   format(user_output, 'cut ==> ', []),
            jsonrpc_call('cut', Id, Parsed9, ProcessIn, ProcessOut),
            (   Parsed9 = success(Result9) ->
                format(user_output, 'Result=~q~n', [Result9])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed9])
            )
        ),
        nl(user_output),
        % Backtrack until failure
        (   Goal5 = 'member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.',
            format(user_output, 'Multiplier=10, call(~q). ==> ', [Goal5]),
            jsonrpc_call('call', json([goal=Goal5,bindings=json(['Multiplier'=10])]), Id, Parsed10, ProcessIn, ProcessOut),
            (   Parsed10 = success(Result10) ->
                format(user_output, 'First Result=~q~n', [Result10])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed10])
            )
        ),
        % Ask for the next solution
        (   format(user_output, 'retry ==> ', []),
            jsonrpc_call('retry', Id, Parsed11, ProcessIn, ProcessOut),
            (   Parsed11 = success(Result11) ->
                format(user_output, '(next) Result=~q~n', [Result11])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed11])
            )
        ),
        % Ask for the next solution
        (   format(user_output, 'retry ==> ', []),
            jsonrpc_call('retry', Id, Parsed12, ProcessIn, ProcessOut),
            (   Parsed12 = success(Result12) ->
                format(user_output, '(next) Result=~q~n', [Result12])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed12])
            )
        ),
        % Ask for the next solution (this will fail, since there are only 3 elements in the list)
        (   format(user_output, 'retry ==> ', []),
            jsonrpc_call('retry', Id, Parsed13, ProcessIn, ProcessOut),
            (   Parsed13 = success(Result13) ->
                format(user_output, '(next) Result=~q~n', [Result13])
            ;   Parsed13 = failed ->
                format(user_output, 'Prolog failed (this is expected)~n', [])
            ;   otherwise ->
                format(user_error, '! UNEXPECTED ~q~n', [Parsed13])
            )
        ),
        nl(user_output),
        % once(foo is bar). This will throw an exception in Prolog, which translates into an error reply.
        (   Goal6 = 'foo is bar.',
            format(user_output, 'once(~q). ==> ', [Goal6]),
            jsonrpc_call('once', json([goal=Goal6]), Id, Parsed14, ProcessIn, ProcessOut),
            (   Parsed14 = success(Result14) ->
                format(user_output, 'Result=~q~n', [Result14])
            ;   Parsed14 = failed ->
                format(user_output, 'failed~n', [])
            ;   Parsed14 = exception(Message1,Data1) ->
                format(user_output, 'Prolog threw an exception (this is expected): ~w ~q~n', [Message1,Data1])
            ;   otherwise ->
                format(user_error, '! UNEXPECTED ~q~n', [Parsed14])
            )
        ),
        nl(user_output),
        % Quit the server
        (   format(user_output, 'quit ==> ', []),
            jsonrpc_call('quit', Id, Parsed15, ProcessIn, ProcessOut),
            (   Parsed15 = success(Result15) ->
                format(user_output, 'Result=~q~n', [Result15])
            ;   format(user_error, '! UNEXPECTED ~q~n', [Parsed15])
            )
        ).


:- dynamic logging/0.
:- volatile logging/0.
% logging.

% Turn on logging.
jsonrpc_client_logging_on :-
        jsonrpc_client_logging_off,
        asserta(logging).

% Turn off logging.
jsonrpc_client_logging_off :-
        retractall(logging).

log(FormatControl, FormatArgs) :-
        logging,
        !,
        format(user_error, 'CLIENT: ', []),
        format(user_error, FormatControl, FormatArgs),
        nl(user_error),
        flush_output(user_error).
log(_FormatControl, _FormatArgs) :-
        true.

:- public log_call/1.           % unused
log_call(X) :-
        logging,
        !,
        once(X).
log_call(_) :-
        true.

:- initialization jsonrpc_client_main, halt.

