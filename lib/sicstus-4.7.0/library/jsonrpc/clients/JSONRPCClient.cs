using System;
using System.Diagnostics;
using System.IO;
using System.Text;

// Use the JSON.NET parser
// Install with: dotnet add package Newtonsoft.Json
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;

namespace JSONRPCClient {
        class Program {

                // The code used by jsonrpc_server.pl to indicate failure.
                private const int FAILURE_CODE = -4711;
                // The code used by jsonrpc_server.pl to indicate a Prolog exception.
                private const int EXCEPTION_CODE = -4712;
                private static string envLogging = Environment.GetEnvironmentVariable ("LOGGING");
                private static Boolean loggingFromEnvironment = false;
                private static bool defaultLogging = true;
                private static bool logging = (Boolean.TryParse (envLogging, out loggingFromEnvironment) ? loggingFromEnvironment : defaultLogging);
                private static bool errorLogging = logging || true; // always log unexpected results

                static void Main (string[] args) {
                        Process child = LaunchServer (args);

                        interact (child.StandardInput, child.StandardOutput);

                        log ("Waiting for child process to exit");
                        child.WaitForExit ();
                        child.Dispose ();
                        child = null;
                }

                private static Process LaunchServer (string[] args) {
                        string defaultExePath;
                        PlatformID platform = Environment.OSVersion.Platform;
                        if (platform == PlatformID.Win32NT) {
                                defaultExePath = ".\\sicstus.exe";
                        } else {
                                defaultExePath = "./sicstus";
                        }

                        string exePath = (args.Length >= 1 ? args[0] : defaultExePath);
                        string plPath = (args.Length >= 2 ? args[1] : "$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server");

                        ProcessStartInfo startInfo = new ProcessStartInfo ();
                        startInfo.FileName = exePath;
                        if (!logging) {
                                startInfo.ArgumentList.Add ("--nologo");
                                startInfo.ArgumentList.Add ("--noinfo");
                        }

                        startInfo.ArgumentList.Add ("-DSP_CTYPE_STDIO=UTF-8");
                        startInfo.ArgumentList.Add ("-l");
                        startInfo.ArgumentList.Add (plPath);
                        startInfo.ArgumentList.Add ("--goal");
                        startInfo.ArgumentList.Add ("jsonrpc_server_main([call_hook(call)]),halt.");

                        UTF8Encoding utf8NoBOM = new UTF8Encoding (encoderShouldEmitUTF8Identifier: false);

                        // Note that Encoding.UTF8 uses BOM, which is not valid in JSON text.
                        startInfo.StandardInputEncoding = utf8NoBOM;
                        startInfo.StandardOutputEncoding = utf8NoBOM;
                        // startInfo.StandardErrorEncoding = utf8NoBOM;

                        startInfo.RedirectStandardInput = true;
                        startInfo.RedirectStandardOutput = true;

                        return Process.Start (startInfo);
                }

                static void interact (System.IO.StreamWriter childStandardInput, System.IO.StreamReader childStandardOutput) {
                        JToken json;
                        JToken result;
                        int id = 0;

                        // Read the initial state (it is null).
                        InfoWrite ("state ==> ");
                        SendRequest (childStandardInput, id++, "state");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);

                        if (result != null) {
                                InfoWrite ("State is " + ToString (result) + "\n");
                        }

                        // Set the state (to 4) and return its previous value.
                        InfoWrite ("state:=4 ==> ");
                        SendRequest (childStandardInput, id++, "state", "[4]");

                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("State was " + ToString (result) + "\n");
                        }

                        // Read current state.
                        InfoWrite ("state ==> ");
                        SendRequest (childStandardInput, id++, "state");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("State is " + ToString (result) + "\n");
                        }
                        InfoWrite ("\n");

                        // Increment current state by 1.
                        InfoWrite ("once(Result is StateIn+1, StateOut=Result). ==> ");
                        SendRequest (childStandardInput, id++, "once", @"{""goal"":""Result is StateIn+1, StateOut=Result.""}");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("Result=" + ToString (result) + "\n");
                        }

                        // Increment current state by 1 (again).
                        InfoWrite ("once(Result is StateIn+1, StateOut=Result). ==> ");
                        SendRequest (childStandardInput, id++, "once", @"{""goal"":""Result is StateIn+1, StateOut=Result.""}");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("Result=" + ToString (result) + "\n");
                        }
                        InfoWrite ("\n");

	                // Increment current state by 5.
                        InfoWrite ("Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> ");
                        SendRequest (childStandardInput, id++, "call", @"{
                                ""goal"":""Result is StateIn+Increment, StateOut=Result."",
                                ""bindings"":{""Increment"":5}}");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("Result=" + ToString (result) + "\n");
                        }
                        InfoWrite ("\n");

                        // Call member(...), backtracking over solutions.
                        InfoWrite ("Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ");
                        SendRequest (childStandardInput, id++, "call", @"{
                                ""goal"":""member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc."",
                                ""bindings"":{""Multiplier"":10}}");

                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("First Result=" + ToString (result) + "\n");
                        }

                        // Ask for the next solution
                        InfoWrite ("retry ==> ");
                        SendRequest (childStandardInput, id++, "retry");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("(next) Result=" + ToString (result) + "\n");
                        }

                        // Commits to the most recent solution of the most recent ""call""
                        InfoWrite ("cut ==> ");
                        SendRequest (childStandardInput, id++, "cut");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("Result=" + ToString (result) + "\n");
                        }
                        InfoWrite ("\n");

                        // Backtrack until failure.
                        InfoWrite ("Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> ");
                        SendRequest (childStandardInput, id++, "call", @"{
                                ""goal"":""member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc."",
                                ""bindings"":{""Multiplier"":10}}");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("First Result=" + ToString (result) + "\n");
                        }

                        // Ask for the next solution
                        InfoWrite ("retry ==> ");
                        SendRequest (childStandardInput, id++, "retry");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("(next) Result=" + ToString (result) + "\n");
                        }

                        // Ask for the next solution
                        InfoWrite ("retry ==> ");
                        SendRequest (childStandardInput, id++, "retry");
                        json = ReadJSON (childStandardOutput);
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("(next) Result=" + ToString (result) + "\n");
                        }

                        // Ask for the next solution (this will fail, since there are only 3 elements in the list).
                        InfoWrite ("retry ==> ");
                        SendRequest (childStandardInput, id++, "retry");
                        json = ReadJSON (childStandardOutput);
                        result = ParseFailure (json);
                        if (result != null) {
                                InfoWrite ("Prolog failed (this is expected)\n");
                        }
                        InfoWrite ("\n");

                        // once(foo is bar). This will throw an exception in Prolog, which translates into an error reply.
                        InfoWrite ("once(foo is bar). ==> ");
                        json = CallRPC (childStandardInput, childStandardOutput, id++, "once", @"{""goal"":""foo is bar.""}");
                        result = ParseException (json);
                        if (result != null) {
                                InfoWrite ("Prolog threw an exception (this is expected): " +
                                        ToString (GetValue (result, "data", GetValue (result, "message", "Unknown"))) + "\n");
                        }
                        InfoWrite ("\n");

                        // Tell server, i.e. sub-process, to quit.
                        InfoWrite ("quit ==> ");
                        json = CallRPC (childStandardInput, childStandardOutput, id++, "quit");
                        result = ParseResult (json);
                        if (result != null) {
                                InfoWrite ("Result=" + ToString (result) + "\n");
                        }
                }

                // Send a JSON-RPC Request and return the reply, if any.
                // If id is negative the request will be a "Notification", i.e. no reply from server and this method returns null.
                private static JToken CallRPC (StreamWriter childStandardInput, StreamReader childStandardOutput, int id, string method, string parameters) {
                        SendRequest (childStandardInput, id, method, parameters);
                        return ReadJSON (childStandardOutput);
                }

                // Send a JSON-RPC Request and return the reply, if any. 
                // If id is negative the request will be a "Notification", i.e. no reply from server and this method returns null.
                private static JToken CallRPC (StreamWriter childStandardInput, StreamReader childStandardOutput, int id, string method) {
                        return CallRPC (childStandardInput, childStandardOutput, id, method, null);
                }

                // Send a JSON-RPC Request. If id is negative the request will be a "Notification" (i.e. no reply from server).
                // The parameters argument should a JSON-syntax Array or Object, or null if no parameters should be passed to the method.
                private static void SendRequest (StreamWriter childStandardInput, int id, string method, string parameters) {
                        SendJSON (childStandardInput, @"{""jsonrpc"":""2.0""" +
                                // Optional id (no id means "Notification", i.e. the server will not send a reply).
                                (id >= 0 ? @", ""id"":" + id : "") +
                                @", ""method"":""" + method + "\"" +
                                // Optional parameters
                                ((parameters != null) ? @", ""params"":" + parameters : "") +
                                @"}");
                }

                // Send a JSON-RPC Request with no parameters. If id is negative a the request will be a "Notification" (i.e. no reply from server).
                private static void SendRequest (StreamWriter childStandardInput, int id, string method) {
                        SendRequest (childStandardInput, id, method, null);
                }

                // Return the "result" of the reply, if present. Otherwise return null and logs details of the error.
                private static JToken ParseResult (JToken json) {
                        JToken result = GetValue (json, "result", null);

                        if (result != null) {
                                return result;
                        }

                        if (logging) {
                                JToken error = GetValue (json, "error", null);
                                if (error != null) {
                                        JToken jsonCode = GetValue (error, "code", null);
                                        if (jsonCode != null && jsonCode.Type == JTokenType.Integer) {
                                                int errorCode = (int) GetValue (error, "code", null);

                                                if (errorCode == FAILURE_CODE) {
                                                        logError ("Prolog failed");
                                                        return null;
                                                } else if (errorCode == EXCEPTION_CODE) {
                                                        logError ("Prolog threw an exception");
                                                        return null;
                                                }
                                        }
                                }

                                logError ("Unexpected reply " + ToString (json));
                        }

                        return null;
                }

                // Return the non-null error object if the reply is an error signifying that 
                // Prolog failed (which is often not an error). Otherwise return null and logs details of the reply.
                private static JToken ParseFailure (JToken json) {
                        JToken error = GetValue (json, "error", null);
                        if (error != null) {
                                JToken jsonCode = GetValue (error, "code", null);
                                if (jsonCode != null && jsonCode.Type == JTokenType.Integer) {
                                        int errorCode = (int) GetValue (error, "code", null);
                                        if (errorCode == FAILURE_CODE) {
                                                // Expected case.
                                                return error;
                                        } else if (errorCode == EXCEPTION_CODE) {
                                                logError ("Prolog threw an exception");
                                                return null;
                                        }
                                }
                        }

                        if (logging) {
                                logError ("Unexpected reply " + ToString (json));
                        }

                        return null;
                }

                // Return the non-null error object if the reply is an error signifying that
                // Prolog threw an exception. Otherwise return null and logs details of the reply.
                private static JToken ParseException (JToken json) {
                        JToken error = GetValue (json, "error", null);
                        if (error != null) {
                                JToken jsonCode = GetValue (error, "code", null);
                                if (jsonCode != null && jsonCode.Type == JTokenType.Integer) {
                                        int errorCode = (int) GetValue (error, "code", null);
                                        if (errorCode == FAILURE_CODE) {
                                                logError ("Prolog failed");
                                                return null;
                                        } else if (errorCode == EXCEPTION_CODE) {
                                                // Expected case, return any non-null value
                                                return error;
                                        }
                                }
                        }

                        if (logging) {
                                logError ("Unexpected reply " + ToString (json));
                        }

                        return null;
                }

                private static JToken ReadJSON (StreamReader childStandardOutput) {
                        return JToken.Parse (childStandardOutput.ReadLine ());
                }

                private static void SendJSON (System.IO.StreamWriter childStandardInput, string json) {
                        log ("Send: " + json);

                        childStandardInput.WriteLine (json);
                        childStandardInput.Flush ();
                }

                // Format the json, also handling null. For debugging/logging.
                private static string ToString (JToken json) {
                        return (json != null ? json.ToString (Formatting.None) : "<<null>>");
                }

                // Write text to console output
                private static void InfoWrite (string text) {
                        Console.Out.Write (text);
                        Console.Out.Flush ();
                }

                private static void log (string msg) {
                        if (logging) {
                                Console.Error.WriteLine (msg);
                        }
                }

                private static void logError (string msg) {
                        if (logging) {
                                Console.Error.WriteLine ("ERROR: " + msg);
                        }
                }

                // If the argument is a JSON object, and it has a member with the given name,
                // return the value of the member. Otherwise return null.
                static JToken GetValue (JToken jt, string propertyName, JToken defaultValue) {
                        if (jt.Type == JTokenType.Object) {
                                JObject obj = jt.ToObject<JObject> ();
                                if (obj.ContainsKey (propertyName)) {
                                        return obj.GetValue (propertyName);
                                }
                        }
                        return defaultValue;
                }
        }
}
