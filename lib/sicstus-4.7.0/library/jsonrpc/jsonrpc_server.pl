/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:4; tab-width:8; -*- */
% Copyright (C) 2020, RISE Research Institutes of Sweden AB.


% NOTE: Beware of security implications of allowing arbitray calls!
:- module(json_server, [jsonrpc_server_main/0,
                        jsonrpc_server_main/1,
                        jsonrpc_server_main/3]).

:- use_module(library(json), [json_write/3,json_read/3]).
:- use_module(library(codesio), [write_term_to_codes/3,open_codes_stream/2]).

:- dynamic active_goal_request_id/2.
:- volatile active_goal_request_id/2.

% Options must be meta so we know which module the call_hook belongs in.
:- meta_predicate parse_options(:, -).

:- meta_predicate jsonrpc_server_main(:).
:- meta_predicate jsonrpc_server_main(?, ?, :).


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


jsonrpc_server_main :-
        jsonrpc_server_main([]).


jsonrpc_server_main(Options) :-
        jsonrpc_server_main(@(null), _StateOut, Options).

jsonrpc_server_main(StateIn, StateOut, Options) :-
        parse_options(Options, Opts),
        loop(StateIn, StateOut, Opts).


loop(State1, State, Opts) :-
        read_message(RPC, Opts),
        handle_message(RPC, State1, State2, Cont, Opts),
        loop(Cont, State2, State, Opts).


loop(Cont, _State1, _State, _Opts) :-
        var(Cont),
        !,
        fail.
loop(done, State1, State, _Opts) :-
        !,
        State = State1.
loop(continue, State1, State, Opts) :-
        loop(State1, State, Opts).

% Succeeds with ContOut = cut(...) if it receives a request to cut an active goal
% Succeeds with ContOut = done if it receives a request to quit
% Fails if it receives a request to retry an active goal
recurse_loop(Cont, _ContOut, _State1, _State, _Opts) :-
        var(Cont),
        !,
        fail.
recurse_loop(done, ContOut, State1, State, _Opts) :-
        !,
        ContOut = done,
        State = State1.
recurse_loop(Cont, ContOut, State1, State, _Opts) :-
        Cont = cut(_),
        !,
        ContOut = Cont,
        State = State1.
recurse_loop(continue, ContOut, State1, State, Opts) :-
        read_message(RPC, Opts),
        handle_message(RPC, State1, State2, Cont, Opts),
        recurse_loop(Cont, ContOut, State2, State, Opts).


handle_message(RPC, StateIn, StateOut, Cont, Opts) :-
        parse_message(RPC, Message, Opts),
        dispatch_message(Message, StateIn, StateOut, Cont, Opts).


dispatch_message(Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(Method,_Id,_Params,_RPC),
        !,
        dispatch_request(Method, Message, StateIn, StateOut, Cont, Opts).
dispatch_message(invalid(RPC), StateIn, StateOut, Cont, Opts) :-
        % Malformed        % FIXME: Logging
        % "Id ... If there was an error in detecting the id in the Request object (e.g. Parse error/Invalid Request), it MUST be Null."
        Id = @(null),
        send_error_reply(Id, invalid_request, RPC, StateIn, StateOut, Opts),
        Cont = done.


dispatch_request('once', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,Id,Params,RPC),
        % If no call hook then we pretend the message is unknown and do no parsing etc.
        opt_value(call_hook, Opts, CallHook),
        nonvar(CallHook),
        !,
        (   parse_json_goal_request(Params, Goal, GoalResult, GoalStateIn, GoalStateOut),
            GoalStateIn = StateIn,
            (   catch(once(call(CallHook, Goal)), error(ErrorTerm,Details), Flag = exception),
                (   Flag == exception ->
                    !,
                    send_exception_reply(Message, Id, error(ErrorTerm,Details), GoalStateIn, StateOut, Opts)
                ;
                    otherwise -> % success
                    % If Goal did not bind the output variables, i.e. Result and StateOut, bind them here.
                    (   var(GoalStateOut) ->
                        GoalStateOut = GoalStateIn
                    ;   true
                    ),
                    (   var(GoalResult) ->
                        GoalResult = @(null)
                    ;   true
                    ),
                    send_success_reply(Message, Id, GoalResult, GoalStateOut, StateOut, Opts)
                ),
                !
            ;
                % Goal failed.
                send_failure_reply(Message, Id, GoalStateIn, StateOut, Opts)
            )
        ;
            % Malformed goal request
            send_error_reply(Id, invalid_params, RPC, StateIn, StateOut, Opts)
        ),
        Cont = continue.

dispatch_request('call', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,CallRequestId,Params,RPC),
        % If no call hook then we pretend the message is unknown and do no parsing etc.
        opt_value(call_hook, Opts, CallHook),
        nonvar(CallHook),
        !,
        (   parse_json_goal_request(Params, Goal, GoalResult, GoalStateIn, GoalStateOut),
            GoalStateIn = StateIn,
            RecStack = [ActiveGoal|Stack],
            opt_value(stack, Opts, Stack),
            ActiveGoal = _UniqueVariable,
            with_option(stack, RecStack, Opts, RecOpts),
            active_goal_key(RecOpts, ActiveGoalKey),
            retractall(active_goal_request_id(ActiveGoalKey, _)),
            asserta(active_goal_request_id(ActiveGoalKey, CallRequestId)),
            (   catch(call(CallHook, Goal), error(ErrorTerm,Details), Flag = exception),
                % Success or exception from Goal
                (   Flag == exception ->
                    !,
                    % The Id can be from the initial 'call' request or from a subsequent 'retry' reguest.
                    retract(active_goal_request_id(ActiveGoalKey, Id)),
                    send_exception_reply(Message, Id, error(ErrorTerm,Details), GoalStateIn, State2, Opts),
                    StateOut = State2,
                    Cont = continue
                ;   otherwise -> % success
                    % If Goal did not bind the output variables, i.e. Result and StateOut, bind them here.
                    (   var(GoalStateOut) ->
                        GoalStateOut = GoalStateIn
                    ;   true
                    ),
                    (   var(GoalResult) ->
                        GoalResult = @(null)
                    ;   true
                    ),
                    % The Id can be from the initial 'call' request or from a subsequent 'retry' reguest.
                    retract(active_goal_request_id(ActiveGoalKey, Id)),
                    send_success_reply(Message, Id, GoalResult, GoalStateOut, State1, Opts),
                    % Recurse loop will fail if it receives a request to retry Goal.
                    recurse_loop(continue, RecCont, State1, State2, RecOpts),
                    (   RecCont = cut(CutTo),
                        CutTo == ActiveGoal ->
                        % Recurse loop was asked to cut this Goal.
                        !,
                        Cont = continue
                    ;   otherwise ->
                        % Possibly cut to some earlier goal (NYI), or 'done'.
                        Cont = RecCont
                    ),
                    StateOut = State2
                ),
                !
            ;
                % Goal failed. Also happens when 'retry' message requested a new solution and found none.
                % The Id can be from the initial 'call' request or from a subsequent 'retry' reguest.
                retract(active_goal_request_id(ActiveGoalKey, Id)),
                send_failure_reply(Message, Id, GoalStateIn, StateOut, Opts),
                Cont = continue
            )
        ;
            % Malformed goal request
            send_error_reply(CallRequestId, invalid_params, RPC, StateIn, StateOut, Opts),
            Cont = continue
        ).

dispatch_request('retry', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,Id,_Params,RPC),
        !,
        opt_value(stack, Opts, Stack),
        (   Stack = [_ActiveGoal|_Stack1] ->
            active_goal_key(Opts, ActiveGoalKey),
            % Tell caller where to send the reply
            asserta(active_goal_request_id(ActiveGoalKey, Id)),
            % fail into caller, to get the next solution
            fail
        ;   otherwise ->
            % No active call. This is an error
            send_error_reply(Id, no_active_call, RPC, StateIn, StateOut, Opts)
        ),
        Cont = continue.

dispatch_request('cut', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,Id,_Params,RPC),
        !,
        opt_value(stack, Opts, Stack),
        (   Stack = [ActiveGoal|_Stack1] ->
            send_ack_reply(Message, Id, StateIn, StateOut, Opts),
            Cont = cut(ActiveGoal)
        ;   otherwise ->
            % No active call. This is an error
            send_error_reply(Id, no_active_call, RPC, StateIn, StateOut, Opts),
            Cont = continue
        ).

dispatch_request('state', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,Id,Params,_RPC),
        !,
        % If Params is empty, just query the state
        (   Params = [NewState] ->
            true
        ;   NewState = StateIn
        ),
        Result = StateIn,       % Return old state
        send_success_reply(Message, Id, Result, NewState, StateOut, Opts),
        Cont = continue.

dispatch_request('quit', Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_Method,Id,_Params,_RPC),
        !,
        Result = 'Bye',
        send_success_reply(Message, Id, Result, StateIn, State1, Opts),
        StateOut = State1,
        Cont = done.

dispatch_request(_Method, Message, StateIn, StateOut, Cont, Opts) :-
        Message = request(_,Id,_Params,RPC),
        !,
        % FIXME: Logging
        send_error_reply(Id, method_not_found, RPC, StateIn, StateOut, Opts),
        Cont = done.


send_ack_reply(Message, Id, StateIn, StateOut, Opts) :-
        send_success_reply(Message, Id, @(null), StateIn, StateOut, Opts).


active_goal_key(RecOpts, ActiveGoalKey) :-
        opt_value(stack, RecOpts, RecStack1),
        length(RecStack1, ActiveGoalKey).


send_error_reply(Id, ErrorCode, Data, StateIn, StateOut, Opts) :-
        error_object_code(ErrorCode, NumericErrorCode, ErrorMessage),
        send_error_reply(Id, NumericErrorCode, ErrorMessage, Data, StateIn, StateOut, Opts).


send_error_reply(Id, ErrorCode, ErrorMessage, Data, StateIn, StateOut, Opts) :-
        nonvar(Id),
        !,
        (   integer(ErrorCode) ->
            NumericErrorCode = ErrorCode
        ;   error_object_code(ErrorCode, NumericErrorCode)
        ),
        (   nonvar(Data) ->
            jsonrpc_error(NumericErrorCode, ErrorMessage, Data, RPCError)
        ;   otherwise ->
            jsonrpc_error(NumericErrorCode, ErrorMessage, RPCError)
        ),
        jsonrpc_error_response(RPCError, Id, RPCResult),
        write_message(RPCResult, Opts),
        StateOut = StateIn.
send_error_reply(_Id, _ErrorCode, _ErrorMessage, _Data, StateIn, StateOut, _Opts) :-
        % var(Id) implies a notification, so no reply should be sent.
        StateOut = StateIn.


send_failure_reply(_Message, Id, State1, State, Opts) :-
        nonvar(Id),
        !,
        failure_error_response(Id, JSONResponse),
        write_message(JSONResponse, Opts),
        State = State1.
send_failure_reply(_Message, _Id, State1, State, _Opts) :-
        % var(Id) implies a notification, so no reply should be sent.
        State = State1.


send_exception_reply(_Message, Id, Exception, State1, State, Opts) :-
        nonvar(Id),
        !,
        Exception = error(ErrorTerm,_Details),
        exception_error_response(Id, JSONResponse, ErrorTerm),
        write_message(JSONResponse, Opts),
        State = State1.
send_exception_reply(_Message, _Id, _Exception, State1, State, _Opts) :-
        % var(Id) implies a notification, so no reply should be sent.
        State = State1.


send_success_reply(_Message, Id, GoalResult, State1, State, Opts) :-
        nonvar(Id),
        !,
        jsonrpc_response(GoalResult, Id, JSONResponse),
        write_message(JSONResponse, Opts),
        State = State1.
send_success_reply(_Message, _Id, _GoalResult, State1, State, _Opts) :-
        % var(Id) implies a notification, so no reply should be sent.
        State = State1.


read_message(RPC, Opts) :-
        opt_value(in, Opts, In),
        opt_value(read_options, Opts, ReadOptions),
        json_read(In, RPC, ReadOptions).


write_message(JSON, Opts) :-
        opt_value(out, Opts, Out),
        opt_value(write_options, Opts, WriteOptions),
        json_write(Out, JSON, WriteOptions),
        % Terminate the line (assuming WriteOptions contains compact(true), but is harmless otherwise).
        nl(Out),
        flush_output(Out).


parse_message(RPC, Message, _Opts) :-
        json_member(RPC, 'method', Method),
        json_member(RPC, 'id', Id, _NoId),
        json_member(RPC, 'params', Params, []),
        !,
        (   var(Id) ->
            Message = notification(Method,Params,RPC)
        ;
            Message = request(Method,Id,Params,RPC)
        ).
parse_message(RPC, Message, _Opts) :-
        % RPC is not valid JSON-RPC 2.0
        Message = invalid(RPC).


exception_error_response(Id, Response, E) :-
        error_object_code(exception, EXCEPTION_CODE),
        writeq_to_atom(E, Exception),
        jsonrpc_error(EXCEPTION_CODE, 'Exception', Exception, RPCError),
        jsonrpc_error_response(RPCError, Id, Response).


failure_error_response(Id, Response) :-
        error_object_code(failure, FAILURE_CODE),
        jsonrpc_error(FAILURE_CODE, 'Failure', RPCError),
        jsonrpc_error_response(RPCError, Id, Response).


writeq_to_atom(Term, Atom) :-
        write_term_to_codes(Term, ExceptionCodes, [quoted(true)]),
        atom_codes(Atom, ExceptionCodes).


error_object_code(Name, Code) :-
        error_object_code(Name, Code, _Description).

error_object_code(parse_error, -32600, 'Invalid JSON was received by the server.').
error_object_code(invalid_request, -32600, 'The JSON sent is not a valid Request object.').
error_object_code(method_not_found, -32601, 'The method does not exist / is not available.').
error_object_code(invalid_params, -32602, 'Invalid method parameter(s).').
error_object_code(internal_error, -32603, 'Internal JSON-RPC error.').


error_object_code(failure, -4711, 'Failure').
error_object_code(exception, -4712, 'Exception').
error_object_code(no_active_call, -4713, 'No active call').


% Fails if malformed argument
parse_json_goal_request(Params, Goal, GoalResult, GoalStateIn, GoalStateOut) :-
        % Module. Defaults to 'user' module if parameter array, and if object without "module" member.
        json_member(Params, module, Module, user),
        (   Params = [F|ParamList] ->
            Goal_1 =.. [F|ParamList],
            Goal = Module:call(Goal_1,GoalResult,GoalStateIn,GoalStateOut)
        ;   Params = json(_) -> % an object
            (   json_member(Params, predicate, F),
                json_member(Params, args, ParamList, []) ->
                Goal_1 =.. [F|ParamList],
                Goal = Module:call(Goal_1,GoalResult,GoalStateIn,GoalStateOut)
            ;   json_member(Params, goal, GoalSpec) ->
                (   atom(GoalSpec) ->
                    atom_codes(GoalSpec, GoalCodes)
                ;   GoalCodes = [_|_],
                    GoalCodes = GoalSpec
                ),
                (   json_member(Params, bindings, json(ParamsBindings)) ->
                    true
                ;   ParamsBindings = []
                ),
                Bindings = ['Result'=GoalResult,
                            'StateIn'=GoalStateIn,
                            'StateOut'=GoalStateOut
                           |ParamsBindings],
                goal_from_codes(GoalCodes, Goal, Bindings)
            ;   otherwise ->    % invalid argument
                fail
            )
        ;   otherwise ->        % invalid argument
            fail
        ).


% The codes should form valid Prolog term syntax (including the terminating full stop (.)).
% If the Prolog term has a variable named "Result" it will be bound the the corresponding argument. Similarly for "StateIn" and "StateOut".
% If the Prolog term does not have a variable named "Result", the Result argument will be bound to the JSON term @(null).
% If the Prolog term does not have a variable named "StateOut", the StateOut argument will be bound to the StateIn argument.
%
% Examples:
% goal_from_codes("hello(world).", Goal, Result, StateIn, StateOut).        ==> Goal = hello(world), Result= @(null), StateOut=StateIn
%
% goal_from_codes("hello(world,StateIn,Result,StateOut).", Goal, A, B, C).  ==> Goal = hello(world,B,A,C) % Note that the substition is by name, not by position
%
% goal_from_codes("hello(world,Result).", Goal, TheResult, StateIn, StateOut). ==> Goal = hello(world,TheResult), StateOut=StateIn
%
% The codes should form valid Prolog term syntax (including the terminating full stop (.)).
% Bindings is in the JSON members list, i.e. [Name=Value, ...] where each Name is an atom (corresponding to a JSON string)
% Variables in the goal will be bound to the corresponding (JSON) value if the variable name is present in Bindings.
goal_from_codes(Codes, Goal, Bindings) :-
        open_codes_stream(Codes, Stream),
        call_cleanup(read_term(Stream, Term0, [variable_names(Variables)]), close(Stream)),
        bind_goal_variables(Bindings, Variables),
        Goal = Term0.


bind_goal_variables([], _Variables).
bind_goal_variables([Name=Value|Bindings], Variables) :-
        memberchk(Name=V, Variables),
        !,
        V = Value,
        bind_goal_variables(Bindings, Variables).
bind_goal_variables([_NameValue|Bindings], Variables) :-
        bind_goal_variables(Bindings, Variables).


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


parse_options(M:Options, Opts) :-
        default_options(Opts1),
        parse_options1(Options, M, Opts1, Opts).


parse_options1(Options, _M, _Opts1, _Opts) :-
        % FIXME: Error handling
        var(Options),
        !,
        fail.
parse_options1([], _M, Opts1, Opts) :-
        Opts = Opts1.
parse_options1([Option|Options], M, Opts1, Opts) :-
        parse_option(Option, M, Opts1, Opts2),
        parse_options1(Options, M, Opts2, Opts).


parse_option(Option, _M, _Opts, _Opts1) :-
        % FIXME: Error handling
        var(Option),
        !,
        fail.
parse_option(in(Value), _M, Opts1, Opts) :-
        with_option(in, Value, Opts1, Opts).
parse_option(out(Value), _M, Opts1, Opts) :-
        with_option(out, Value, Opts1, Opts).
parse_option(read_options(Value), _M, Opts1, Opts) :-
        with_option(read_options, Value, Opts1, Opts).
parse_option(write_options(Value), _M, Opts1, Opts) :-
        with_option(write_options, Value, Opts1, Opts).
parse_option(stack(Value), _M, Opts1, Opts) :-
        fail,                   % Internal option
        with_option(stack, Value, Opts1, Opts).
parse_option(call_hook(Value), M, Opts1, Opts) :-
        with_option(call_hook, M:Value, Opts1, Opts).


with_option(Key, _Value, _Opts1, _Opts) :-
        var(Key),
        !,
        fail.
with_option(in, Value, opt6(_In,Out,ReadOptions,WriteOptions,Stack,CallHook), opt6(Value,Out,ReadOptions,WriteOptions,Stack,CallHook)).
with_option(out, Value, opt6(In,_Out,ReadOptions,WriteOptions,Stack,CallHook), opt6(In,Value,ReadOptions,WriteOptions,Stack,CallHook)).
with_option(read_options, Value, opt6(In,Out,_ReadOptions,WriteOptions,Stack,CallHook), opt6(In,Out,Value,WriteOptions,Stack,CallHook)).
with_option(write_options, Value, opt6(In,Out,ReadOptions,_WriteOptions,Stack,CallHook), opt6(In,Out,ReadOptions,Value,Stack,CallHook)).
with_option(stack, Value, opt6(In,Out,ReadOptions,WriteOptions,_,CallHook), opt6(In,Out,ReadOptions,WriteOptions,Value,CallHook)).
with_option(call_hook, Value, opt6(In,Out,ReadOptions,WriteOptions,Stack,_), opt6(In,Out,ReadOptions,WriteOptions,Stack,Value)).


default_options(opt6(In,Out,ReadOptions,WriteOptions,Stack,CallHook)) :-
        default_option(in, In),
        default_option(out, Out),
        default_option(read_options, ReadOptions),
        default_option(write_options, WriteOptions),
        default_option(stack, Stack),
        default_option(call_hook, CallHook).

default_option(in, In) :-
        current_input(In).
default_option(out, Out) :-
        current_output(Out).
default_option(read_options, []).
% Compact JSON makes it possible for the client to read each reply as a single line.
default_option(write_options, [compact(true)]).
default_option(stack, []).
default_option(call_hook, _NoCallHook). % A variable signifies the absense of call/once -support.


% Get the option value associated with key
:- opt_value(+Key, +Opts, -Value) is det.
opt_value(Key, _Opts, _Value) :-
        var(Key),
        !,
        fail.
opt_value(in, opt6(In,_Out,_ReadOptions,_WriteOptions,_Stack,_CallHook), In).
opt_value(out, opt6(_In,Out,_ReadOptions,_WriteOptions,_Stack,_CallHook), Out).
opt_value(read_options, opt6(_In,_Out,ReadOptions,_WriteOptions,_Stack,_CallHook), ReadOptions).
opt_value(write_options, opt6(_In,_Out,_ReadOptions,WriteOptions,_Stack,_CallHook), WriteOptions).
opt_value(stack, opt6(_In,_Out,_ReadOptions,_WriteOptions,Stack,_CallHook), Stack).
opt_value(call_hook, opt6(_In,_Out,_ReadOptions,_WriteOptions,_Stack,CallHook), CallHook).
