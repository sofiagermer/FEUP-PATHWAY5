# Copyright (C) 2020, RISE Research Institutes of Sweden AB.

# Usage: python jsonrpc_client.py [sicstus_path [ jsonrpc_server.pl_path ] ]
#
# The program starts the sicstus sub-process and sends some requests to
# Prolog. Finally it tells the sicstus sub-process to exit, and quits.


# Example transcript on macOS/Linux (Windows is similar), assuming sicstus is on PATH
#
#    $ SRC_DIR="$( sicstus --goal "absolute_file_name(library('jsonrpc/clients'),D), write(D), nl, halt." 2>/dev/null )"
#    $ python3 "${SRC_DIR}/jsonrpc_client.py"
#    state ==> State is None
#    state:=4 ==> State was None
#    state ==> State is 4
#
#    once(Result is StateIn+1, StateOut=Result). ==> Result=5
#    once(Result is StateIn+1, StateOut=Result). ==> Result=6
#
#    Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> Result=11
#
#    Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
#    retry ==> (next) Result=211
#    cut ==> Result=None
#
#    Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
#    retry ==> (next) Result=211
#    retry ==> (next) Result=311
#    retry ==> Prolog failed (this is expected)
#
#    once(foo is bar). ==> Prolog threw an exception (this is expected): msg=Exception, data=type_error(evaluable,bar/0)
#
#    quit ==> Result=Bye
#    $

import os
import sys
import json
import subprocess

default_logging = False
logging = (os.environ.get('LOGGING', str(default_logging)).lower() in ["true"])

def log(arg):
    if logging:
        print("Log: " + str(arg))
    return arg


# Some sample JSON-RPC 2.0 requests

# Read the state
request_state_read = json.dumps({'jsonrpc':'2.0','id':1
                                 ,'method':'state'})

# Set the state to a JSON object (4).
request_state_set4 = json.dumps({'jsonrpc':'2.0','id':2
                                 ,'method':'state', 'params':[4]})

# Increment the state with 1
request_increment1 = json.dumps({'jsonrpc':'2.0','id':3
                                 # Like: once(Result is StateIn+1).
                                 # and then lets the new state be the value of Result
                                 ,'method':'once', 'params':{'goal':"Result is StateIn+1, StateOut=Result."}})


# Increment the state with an increment specified as a VariableName:Value pair
request_increment5 = json.dumps({'jsonrpc':'2.0','id':4
                                # 'once' only computes the first solution, just like once/1 in Prolog.
                                ,'method':'once'
                                # Like: Increment=5, once(Result is StateIn+Increment).
                                # and then lets the new state be the value of Result
                                , 'params':{'goal':"Result is StateIn+Increment,StateOut=Result."
                                            ,'bindings':{'Increment':5}}})

# Call a goal and return the first solution. Backtrac over results with "retry" request. Commit to a result with "cut" request
request_member = json.dumps({'jsonrpc':'2.0','id':5 ,
                             # 'call' computes each solution on demand, until failure or cut, just like call/1 in Prolog.
                             'method':'call'
                             # Like: Multiplier=10, call(member(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc)).
                             # Does not bind StateOut, so the state will not change.
                             , 'params':{'goal':"member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc."
                                         ,'bindings':{'Multiplier':10}}})

# Ask for a new solution to the most recent 'call'
request_retry = json.dumps({'jsonrpc':'2.0','id':6
                            # This is like ';' when the Prolog toplevel presents a solution
                            ,'method':'retry'})

# Commits to the most recent solution of the most recent 'call'
request_cut = json.dumps({'jsonrpc':'2.0','id':7
                          # This is like !/0
                          ,'method':'cut'})

# Tell the Prolog server to stop reading requests
request_quit = json.dumps({'jsonrpc':'2.0','id':6 ,'method':'quit'})


# A 'once' request that throws an exception ('call' and 'retry' can also do this)
request_exception = json.dumps({'jsonrpc':'2.0','id':7
                                # Like: once(foo is bar).
                                # This will throw an error in Prolog
                                ,'method':'once', 'params':{'goal':"foo is bar."}})

# Default parameters
exe_path = 'sicstus' # default (looked up in $PATH)
pl_path = '$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server'

# Make it possible to pass path to SICStus, and path to Prolog code, on command line
if __name__ == "__main__":
    if len(sys.argv) > 1:
        exe_path = sys.argv[1];
    if len(sys.argv) > 2:
        pl_path = sys.argv[2]


# Start the Prolog server as a sub-process
# The Prolog stderr will go to the console (which is helpful during troubleshooting), unless you set stderr=subprocess.NULL
# SICStus will be less verbose if you pass --nologo and/or --noinfo
#
proc = subprocess.Popen([exe_path
                         ,'-l', pl_path
                         ,'--goal', 'jsonrpc_server_main([call_hook(call)]),halt.'
                         ] + ( [] if logging else ['--nologo' ,'--noinfo'])

                        ,stdout=subprocess.PIPE
                        ,stdin=subprocess.PIPE
                        ,stderr=(None if logging else subprocess.DEVNULL)
                        ,encoding='UTF-8'
                        )

# Read the initial state (it is null/Node)
print("state ==> ", end="")
proc.stdin.write(request_state_read); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("State is " + str(x["result"]))

# Set the state (to 4) and return its previous value
print("state:=4 ==> ", end="")
proc.stdin.write(request_state_set4); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("State was " + str(x["result"]))

# Read current state
print("state ==> ", end="")
proc.stdin.write(request_state_read); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("State is " + str(x["result"]))
print()

# Increment current state by 1
print("once(Result is StateIn+1, StateOut=Result). ==> ", end="")
proc.stdin.write(request_increment1); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("Result=" + str(x["result"]))

# Increment current state by 1 (again)
print("once(Result is StateIn+1, StateOut=Result). ==> ", end="")
proc.stdin.write(request_increment1); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("Result=" + str(x["result"]))
print()

# Increment current state by 1 (again)
print("Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> ", end="")
proc.stdin.write(request_increment5); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("Result=" + str(x["result"]))
print()

# Call member(...), backtracking over solutions
print("Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ", end="")
proc.stdin.write(request_member); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("First Result=" + str(x["result"]))

# Ask for the next solution
print("retry ==> ", end="")
proc.stdin.write(request_retry); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("(next) Result=" + str(x["result"]))

# Cut, committing to the last solution
print("cut ==> ", end="")
proc.stdin.write(request_cut); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("Result=" + str(x["result"]))
print()

# Backtrack until failure
print("Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ", end="")
proc.stdin.write(request_member); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("First Result=" + str(x["result"]))

# Ask for the next solution
print("retry ==> ", end="")
proc.stdin.write(request_retry); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("(next) Result=" + str(x["result"]))

# Ask for the next solution
print("retry ==> ", end="")
proc.stdin.write(request_retry); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("(next) Result=" + str(x["result"]))

# Ask for the next solution (this will fail, since there are only 3 elements in the list)
print("retry ==> ", end="")
proc.stdin.write(request_retry); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r);
log(x)
if "error" in x:
    # Error, most likely failure:
    log("Error: " + str(x))
    err = x["error"]
    code = err["code"]
    if code == -4711:
        print("Prolog failed (this is expected)")
    else:
        msg = err["message"]
        data = err.get("data", None) # data member may be absent
        print("Some other error: code=" + str(code) + ", msg=" + str(msg) + ", data=" + str(data))

else:
    # Success,
    print("(next) Result=" + str(x["result"]))
print()

# once(foo is bar). This will throw an exception in Prolog, which translates into an error reply.
print("once(foo is bar). ==> ", end="")
proc.stdin.write(request_exception); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r);
log(x)
if "error" in x:
    # Error
    log("Error: " + str(x))
    err = x["error"]
    code = err["code"]
    if code == -4711:
        print("Prolog failed")
    elif code == -4712:
        msg = err["message"]
        data = err.get("data", None) # data member may be absent in error objects (but will not be for exception replies from Prolog)
        print("Prolog threw an exception (this is expected): " + "msg=" + str(msg) + ", data=" + str(data))
    else:
        msg = err["message"]
        data = err.get("data", None) # data member may be absent
        print("Some other error: code=" + str(code) + ", msg=" + str(msg) + ", data=" + str(data))

else:
    # Success. This is unexpected here
    print("Unexpectedly did not get an error!?" + str(x))
print()

# Quit the server
print("quit ==> ", end="")
proc.stdin.write(request_quit); proc.stdin.flush()
r = proc.stdout.readline(); log("Sent JSON: " + r)
x = json.loads(r); log(x)
print("Result=" + str(x["result"]))

