/* -*- Mode:C; coding:iso-8859-1; indent-tabs-mode:nil; tab-width:8; -*- */
/* Copyright (C) 2020, RISE Research Institutes of Sweden AB. */

/*
Usage: ./jsonrpc_client [sicstus_path [ jsonrpc_server.pl_path ] ]

The program starts the sicstus sub-process and sends some requests to
Prolog. Finally it tells the sicstus sub-process to exit, and quits.

This code is for demonstrational purposes only. It is not necessarilily suitable for production use.
*/

/*
  Example transcripts

  On macOS/Linux, assuming sicstus is on PATH:

    $ SRC_DIR="$( sicstus --goal "absolute_file_name(library('jsonrpc/clients'),D), write(D), nl, halt." 2>/dev/null )"
    $ go run "${SRC_DIR}/jsonrpc_client.go"
    state ==> State is <nil>
    state:=4 ==> State was <nil>
    state ==> State is 4

    once(Result is StateIn+1, StateOut=Result). ==> Result=5
    once(Result is StateIn+1, StateOut=Result). ==> Result=6

    Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> Result=11

    Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
    retry ==> (next) Result=211
    cut ==> Result=<nil>

    Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
    retry ==> (next) Result=211
    retry ==> (next) Result=311
    retry ==> Prolog failed (this is expected)

    once(foo is bar). ==> Prolog threw an exception (this is expected)

    quit ==> Result=Bye
    $ 

  On Windows, assuming jsonrpc_client.go is in the current directory:

    go run ./jsonrpc_client.go

    ... Exactly as in the transcript above ...

*/

package main

import (
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strconv"
)

var (
	// Command line options
	verbose = flag.Bool("verbose", false, "whether to show logging etc to stderr")
)

var (
	/* The code used by jsonrpc_server.pl to indicate failure. The code is
	   passed, in an "error object", as a number. The constant is the
	   (unique) JSON representation of that code. */
	failureCodeString   = "-4711"
	failureCodeValue, _ = strconv.ParseInt(failureCodeString, 10, 64)

	/* The code used by jsonrpc_server.pl to indicate a Prolog exception. */
	exceptionCodeString   = "-4712"
	exceptionCodeValue, _ = strconv.ParseInt(exceptionCodeString, 10, 64)
)

// Representation of a JSONRPC 2.0 Response
type jsonRPCResponse struct {
	ID     uint64      `json:"id"`
	Result interface{} `json:"result"`
	Error  interface{} `json:"error"`
}

func sendJSON(streamToSubProcess io.WriteCloser, json string) {
	io.WriteString(streamToSubProcess, json)
	io.WriteString(streamToSubProcess, "\n")
}

func readResponse(decoder *json.Decoder) (*jsonRPCResponse, error) {
	var response jsonRPCResponse

	if err := decoder.Decode(&response); err != nil {
		return nil, err
	}

	return &response, nil
}

// True iff the Error is a (non-nil) map
func isJSONRPCError(response *jsonRPCResponse) bool {
	if response != nil {
		if _, ok := response.Error.(map[string]interface{}); ok {
			return true
		}
	}
	return false
}

// The Error as a map, or nil
func getJSONRPCError(response *jsonRPCResponse) map[string]interface{} {
	if response != nil {
		if m, ok := response.Error.(map[string]interface{}); ok {
			return m
		}
	}
	return nil
}

// The Error code, as an int64, or zero if there is no error code, or it cannot be converted to a int64.
func getJSONRPCErrorCode(response *jsonRPCResponse) int64 {
	errorResponse := getJSONRPCError(response)
	if errorResponse != nil {
		if code, ok := errorResponse["code"].(json.Number); ok {
			value, err := code.Int64()
			if err == nil {
				return value
			}
		}
	}
	return 0
}

// True iff the response represents the result from a Prolog query that failed.
func isPrologFailure(response *jsonRPCResponse) bool {
	return getJSONRPCErrorCode(response) == failureCodeValue
}

// True iff the response represents the result from a Prolog query that threw an exceptions.
func isPrologException(response *jsonRPCResponse) bool {
	return getJSONRPCErrorCode(response) == exceptionCodeValue
}

// True iff the response represents the result from a Prolog query that threw an exceptions.
func isPrologSuccess(response *jsonRPCResponse) bool {
	// Note that Result can be JSON null on success.
	return response != nil && response.Error == nil
}

func interact(streamToSubProcess io.WriteCloser, streamFromSubProcess io.ReadCloser) error {
	var err error
	var response *jsonRPCResponse

	decoder := json.NewDecoder(streamFromSubProcess)
	decoder.UseNumber()

	// Read the initial state (it is null)
	fmt.Fprintf(os.Stdout, "state ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"state"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "State is %v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Set the state (to 4) and return its previous value.
	fmt.Fprintf(os.Stdout, "state:=4 ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"state", "params":[4]}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "State was %v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Read current state.
	fmt.Fprintf(os.Stdout, "state ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"state"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "State is %v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// Increment current state by 1.
	fmt.Fprintf(os.Stdout, "once(Result is StateIn+1, StateOut=Result). ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"once", "params":{"goal":"Result is StateIn+1, StateOut=Result."}}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Increment current state by 1 (again).
	fmt.Fprintf(os.Stdout, "once(Result is StateIn+1, StateOut=Result). ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"once", "params":{"goal":"Result is StateIn+1, StateOut=Result."}}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// Increment current state by 5.
	fmt.Fprintf(os.Stdout, "Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> ")
	sendJSON(streamToSubProcess,
		`{"jsonrpc": "2.0",
			"id": 1,
			"method": "once",
			"params": {
						"goal": "Result is StateIn+Increment, StateOut=Result.",
					    "bindings": {
							"Increment": 5
						}
					 }
		}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// Call member(...), backtracking over solutions.
	fmt.Fprintf(os.Stdout, "Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ")
	sendJSON(streamToSubProcess,
		`{
			"jsonrpc": "2.0",
			"id": 1,
			"method": "call",
			"params": {
						"goal": "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.",
						"bindings": {
						"Multiplier": 10
					}
			}
		}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "First Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Ask for the next solution
	fmt.Fprintf(os.Stdout, "retry ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":2,"method":"retry"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "(next) Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Commits to the most recent solution of the most recent 'call'
	fmt.Fprintf(os.Stdout, "cut ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":4,"method":"cut"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// Backtrack until failure.
	fmt.Fprintf(os.Stdout, "Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ")
	sendJSON(streamToSubProcess,
		`{
			"jsonrpc": "2.0",
			"id": 1,
			"method": "call",
			"params": {
				"goal": "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc.",
				"bindings": {
					"Multiplier": 10
				}
			}
		}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "First Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Ask for the next solution
	fmt.Fprintf(os.Stdout, "retry ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":2,"method":"retry"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "(next) Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Ask for the next solution
	fmt.Fprintf(os.Stdout, "retry ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":3,"method":"retry"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "(next) Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	// Ask for the next solution (this will fail, since there are only 3 elements in the list).
	fmt.Fprintf(os.Stdout, "retry ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":4,"method":"retry"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologFailure(response) {
		fmt.Fprintf(os.Stdout, "Prolog failed (this is expected)\n")
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// once(foo is bar). This will throw an exception in Prolog, which translates into an error reply.
	fmt.Fprintf(os.Stdout, "once(foo is bar). ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"once", "params":{"goal":"foo is bar."}}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologException(response) {
		fmt.Fprintf(os.Stdout, "Prolog threw an exception (this is expected)\n")
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}
	fmt.Fprintf(os.Stdout, "\n")

	// Tell server, i.e. sub-process, to quit.
	fmt.Fprintf(os.Stdout, "quit ==> ")
	sendJSON(streamToSubProcess, `{"jsonrpc":"2.0","id":1,"method":"quit"}`)
	if response, err = readResponse(decoder); err != nil {
		return err
	}
	if isPrologSuccess(response) {
		fmt.Fprintf(os.Stdout, "Result=%v\n", response.Result)
	} else {
		return fmt.Errorf("Unexpected result from Prolog %v", response)
	}

	return nil
}

func test() error {
	var err error

	program := "sicstus"
	plPath := `$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server`

	if len(flag.Args()) >= 1 {
		program = flag.Args()[0]
		if len(flag.Args()) >= 2 {
			plPath = flag.Args()[1]
		}
	}

	var args []string

	if !*verbose {
		args = append(args, "--nologo")
		args = append(args, "--noinfo")
	}
	args = append(args, "-l", plPath)

	if *verbose {
		args = append(args,
			"--goal",
			"assert((my_call(X) :- format(user_error, '~NIn Server, ~q~n', [call(X)]),flush_output(user_error),"+
				"call(X))),jsonrpc_server_main([call_hook(my_call)]),halt.")
	} else {
		args = append(args,
			"--goal",
			"jsonrpc_server_main([call_hook(call)]),halt.")
	}

	cmd := exec.Command(program, args...)
	var streamToSubProcess io.WriteCloser
	var streamFromSubProcess io.ReadCloser

	if streamToSubProcess, err = cmd.StdinPipe(); err != nil {
		return err
	}
	if streamFromSubProcess, err = cmd.StdoutPipe(); err != nil {
		return err
	}
	cmd.Stderr = os.Stderr

	if err = cmd.Start(); err != nil {
		return err
	}

	return interact(streamToSubProcess, streamFromSubProcess)
}

func main() {
	flag.Usage = func() {
		fmt.Fprintf(flag.CommandLine.Output(), "Usage: %s [sicstus_path [ jsonrpc_server.pl_path ] ]\n", os.Args[0])
		flag.PrintDefaults()
	}

	flag.Parse()
	err := test()
	if err != nil {
		os.Stderr.WriteString(err.Error())
		os.Exit(1)
	}
}
