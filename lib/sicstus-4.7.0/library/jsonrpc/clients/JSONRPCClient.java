import java.io.*;
import java.lang.ProcessBuilder.Redirect;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.Map.Entry;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/*
 * Copyright (C) 2020, RISE Research Institutes of Sweden AB.
 */

/**
 * A simple example of invoking SICStus as a sub-process, and passing data back
 * and forth using JSON-RPC messages. The example includes backtracking over
 * solutions.
 * <p>
 * The example code contains some rudimentary JSON and JSON-RPC functionality
 * but it is not intended for production use. There are many full-featured JSON
 * and JSON-RPC implementations available elsewhere.
 * <p>
 * For questions, contact <tt>sicstus-support@ri.se</tt>.
 */

/*-
 * Example transcript (macOS/Linux but it is similar on Windows):
 *
 *   $ SRC_DIR="$( sicstus --goal "absolute_file_name(library('jsonrpc/clients'),D), write(D), nl, halt." 2>/dev/null )"
 *   $ mkdir json_java_test
 *   $ cd json_java_test
 *
 *   $ javac -d . "${SRC_DIR}/JSONRPCClient.java" && jar cf JSONRPCClient.jar JSONRPCClient*.class
 *   $ java -Dlogging=false -cp JSONRPCClient.jar JSONRPCClient
 *   state ==> State is null
 *   state:=4 ==> State was null
 *   state ==> State is 4
 *
 *   once(Result is StateIn+1, StateOut=Result). ==> Result=5
 *   once(Result is StateIn+1, StateOut=Result). ==> Result=6
 *
 *   Increment=5, once(Result is StateIn+Increment, StateOut=Result). ==> Result=11
 *
 *   Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
 *   retry ==> (next) Result=211
 *   cut ==> Result=null
 *
 *   Multiplier=10, call(member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc). ==> First Result=111
 *   retry ==> (next) Result=211
 *   retry ==> (next) Result=311
 *   retry ==> Prolog failed (this is expected)
 *
 *   once(foo is bar). ==> Prolog threw an exception (this is expected): "type_error(evaluable,bar/0)"
 *
 *   quit ==> Result="Bye"
 *   $ 
 */
@SuppressWarnings({"boxing", "JavaDoc"})
public class JSONRPCClient {
    private static final int PROLOG_ERROR_CODE_FAIL = -4711;
    private static final int PROLOG_ERROR_CODE_EXCEPTION = -4712;

    private static final boolean RUN_TESTS = Boolean.getBoolean("runTests");
    private static final boolean LOGGING = Boolean.getBoolean("logging");


    public static class JSONParser {

        @SuppressWarnings("WeakerAccess")
        public static JSONParser create() {
            return new JSONParser();
        }

        private JSONParser() {

        }

        @SuppressWarnings("WeakerAccess")
        public static class JSONParseError extends RuntimeException {
            private static final long serialVersionUID = 1L;

            @SuppressWarnings("unused")
            JSONParseError(String message, Throwable t) {
                super(message, t);
            }

            JSONParseError(String message) {
                super(message);
            }
        }

        @SuppressWarnings("WeakerAccess")
        public JsoNaive.JSONValue parse(String json) {
            JsoNaive.JSONValue value = new JsoNaive(json).parseValue();
            if (value == null) {
                throw new JSONParseError("Error parsing JSON \"" + json + "\"");
            }
            return value;
        }
    }

    public static class JSONWriter {
        @SuppressWarnings("WeakerAccess")
        public static class JSONWriterError extends RuntimeException {
            private static final long serialVersionUID = 1L;

            JSONWriterError(String message) {
                super(message);
            }
        }

        public static class JSONWrapper {
            private final Object iObject;

            private JSONWrapper(Object o) {
                iObject = o;
            }

            @SuppressWarnings("WeakerAccess")
            public static JSONWrapper create(Object o) {
                return new JSONWrapper(o);
            }

            private String toJSON() {
                return toJSON(new StringBuilder()).toString();
            }

            @SuppressWarnings({"WeakerAccess", "UnnecessaryUnboxing"})
            public StringBuilder toJSON(StringBuilder sb) {
                Object o = iObject;
                if (o instanceof Map<?, ?>) {
                    Map<?, ?> map = (Map<?, ?>) o;
                    sb.append('{');
                    String prefix = "";
                    for (Entry<?, ?> e : map.entrySet()) {
                        Object key = e.getKey();
                        if (key instanceof String) {
                            sb.append(prefix);
                            stringToJSON(sb, (String) key);
                            sb.append(": ");
                            create(e.getValue()).toJSON(sb);
                            prefix = ", ";
                        }
                    }
                    sb.append('}');
                } else if (o instanceof List<?>) {
                    List<?> list = (List<?>) o;
                    sb.append('[');
                    String prefix = "";
                    for (Object e : list) {
                        sb.append(prefix);
                        create(e).toJSON(sb);
                        prefix = ", ";
                    }
                    sb.append(']');
                } else if (o instanceof String) {
                    stringToJSON(sb, (String) o);

                } else if (o instanceof Number) {
                    sb.append(o);
                } else if (o instanceof Boolean) {
                    sb.append(((Boolean) o).booleanValue() ? "true" : "false");
                } else if (o == null) {
                    sb.append("null");
                } else {
                    throw new JSONWriterError("Unhandled type: " + o);
                }
                return sb;
            }

            private void stringToJSON(StringBuilder sb, String s) {
                sb.append('"');
                escapeString(sb, s);
                sb.append('"');
            }

            /**
             * Escape characters, as needed, to make them suitable for a JSON string
             *
             * @param sb
             * @param s
             * @return
             */
            @SuppressWarnings("UnusedReturnValue")
            private StringBuilder escapeString(StringBuilder sb, String s) {
                for (int i = 0, n = s.length(); i < n; i++) {
                    char c = s.charAt(i);
                    if (c <= 0x1F) {
                        escapeChar(sb, c);
                    } else if (c <= 127) {
                        // ASCII, always unescaped
                        sb.append(c);
                    } else {
                        // Escape everything else (this is not necessary, it should be an option)
                        escapeChar(sb, c);
                    }

                }
                return sb;
            }

            @SuppressWarnings("UnusedReturnValue")
            private StringBuilder escapeChar(StringBuilder sb, char c) {
                sb.append("\\u");
                if (c <= 0x0FFF) {
                    sb.append('0');
                }
                if (c <= 0x00FF) {
                    sb.append('0');
                }
                if (c <= 0x000F) {
                    sb.append('0');
                }

                sb.append(Integer.toHexString(c));
                return sb;
            }

            @Override
            public String toString() {
                return "JSONWrapper [iObject=" + iObject + ", JSON=\"" + toJSON() + "\"]";
            }

        }

        @SuppressWarnings("WeakerAccess")
        public static JSONWriter create() {
            return new JSONWriter();
        }

        private JSONWriter() {

        }

        @SuppressWarnings("WeakerAccess")
        public String toJSON(Object o) {
            return JSONWrapper.create(o).toJSON();
        }

    }

    // Returns 0 on success, 1 on failure
    private static int testJSONParser(String jsonText, boolean expectError) {
        JSONParser parser = JSONParser.create();
        JsoNaive.JSONValue value;
        try {
            value = parser.parse(jsonText);
        } catch (JSONParser.JSONParseError e) {
            if (expectError) {
                return 0;
            }
            System.err.println("TEST FAILED: error from parser");
            e.printStackTrace();

            return 1;
        }

        JsoNaive.JSONValue[] a = value.asArray();
        Map<String, JsoNaive.JSONValue> o = value.asObject();
        String s = value.asString();
        Number n = value.asNumber();
        Boolean b = value.asBoolean();
        boolean isNull = value.isNull();

        System.err.println(value + " " + "asArray=" + Arrays.toString(a) + ", asObject=" + o
                + ", asString=" + s + ", asNumber=" + n
                + ", asBoolean=" + b + ", isNull=" + isNull);

        if (expectError) {
            System.err.println("TEST FAILED: Expected error from parser");
            return 1;
        }
        return 0;

    }

    private static int testJSONParser(String jsonText) {
        return testJSONParser(jsonText, false);
    }

    private static int testJSONParser() {
        int failures = 0;
        failures += testJSONParser("{\"foo\":1, \"bar\":\"baz\"}");
        failures += testJSONParser("{\"foo\":1, \"inner\":{\"foo\":1, \"bar\":\"baz\"}}");
        failures += testJSONParser("[\"foo\",1, \"inner\",{\"foo\":1, \"bar\":\"baz\"}]");
        failures += testJSONParser("\"foo\"");
        failures += testJSONParser("42");
        failures += testJSONParser("42.54");
        failures += testJSONParser("true");
        failures += testJSONParser("false");
        failures += testJSONParser("null");
        failures += testJSONParser("{\"jsonrpc\":\"2.0\",\"id\":12,\"error\":{\"code\":-4711,\"message\":\"Failure\"}}");
        // Malformed:
        failures += testJSONParser("{\"foo\":1, 444:{\"foo\":1, \"bar\":\"baz\"}}", true);
        return failures;
    }

    private static int testJSONWriter(Object o, boolean expectError) {
        JSONWriter writer = JSONWriter.create();

        String json;
        try {
            json = writer.toJSON(o);
        } catch (JSONWriter.JSONWriterError e) {
            if (expectError) {
                return 0;
            }
            System.err.println("TEST FAILED: error from writer");
            e.printStackTrace();
            return 1;
        }

        System.err.println(o + "==JSON==> " + json);
        if (expectError) {
            System.err.println("TEST FAILED: Expected error from writer");
            return 1;
        }

        return testJSONParser(json);
    }

    private static int testJSONWriter(Object o) {
        return testJSONWriter(o, false);
    }

    @SuppressWarnings("UnnecessaryBoxing")
    private static int testJSONWriter() {
        int failures = 0;
        failures += testJSONWriter(Integer.valueOf(42));
        failures += testJSONWriter(Double.valueOf(42.34));
        failures += testJSONWriter(null);
        failures += testJSONWriter(Boolean.TRUE);
        failures += testJSONWriter(Boolean.FALSE);
        failures += testJSONWriter("foo");
        failures += testJSONWriter(Collections.singletonList("a"));

        Map<String, Object> map = new LinkedHashMap<>();

        failures += testJSONWriter(map);

        map.clear();
        map.put("foo", Integer.valueOf(45));
        failures += testJSONWriter(map);

        Map<String, Object> map2 = new LinkedHashMap<>();
        map2.put("bork", "blah");
        map2.put("inner", map);
        map2.put("a list", Arrays.asList("a", map, Boolean.TRUE));
        failures += testJSONWriter(map2);

        // Unhandled value type
        failures += testJSONWriter(new Object(), true);

        return failures;
    }

    @SuppressWarnings({"UnnecessaryBoxing"})
    private static String toRPC(int id, String method, Object params) {
        Map<String, Object> request = new LinkedHashMap<>();

        request.put("jsonrpc", "2.0");
        if (id >= 0) {
            request.put("id", Integer.valueOf(id));
        }
        request.put("method", method);
        if (params != null) {
            request.put("params", params);
        }
        return JSONWriter.create().toJSON(request);
    }

    @SuppressWarnings("SpellCheckingInspection")
    private static Process launch(String path, String code_path) {
        List<String> command = new ArrayList<>();
        command.add(path);
        if (!LOGGING) {
            command.add("--nologo");
            command.add("--noinfo");
        }
        command.add("-l");
        command.add(code_path);

        command.add("--goal");
        command.add("jsonrpc_server_main([call_hook(call)]),halt.");

        ProcessBuilder pb = new ProcessBuilder(command);
        pb.redirectError(Redirect.INHERIT);

        Process process;
        try {
            process = pb.start();
        } catch (IOException e) {
            if (LOGGING) {
                e.printStackTrace();
            }
            process = null;
        }
        return process;
    }

    @SuppressWarnings("UnusedReturnValue")
    private static <T> T log(T msg) {
        if (LOGGING) {
            System.err.println(msg);
        }
        return msg;
    }

    private static Map<String, JsoNaive.JSONValue> callRPC(String method, Writer processIn, BufferedReader processOut, int id)
            throws IOException {
        return callRPC(method, null, processIn, processOut, id);
    }

    @SuppressWarnings("StatementWithEmptyBody")
    private static Map<String, JsoNaive.JSONValue> callRPC(String method, Object params, Writer processIn,
                                                           BufferedReader processOut, int id) throws IOException {
        String json;
        Map<String, JsoNaive.JSONValue> obj = null;

        json = toRPC(id, method, params);
        processIn.write(json);
        processIn.write('\n');
        processIn.flush();

        // id >= 0 signifies a request (and we should get a response).
        if (id >= 0) {
            String reply = processOut.readLine();
            log("reply: " + reply);

            JsoNaive.JSONValue value = JSONParser.create().parse(reply);
            log("value: " + value);
            obj = value.asObject();
            if (obj != null) {
                JsoNaive.JSONValue result = obj.get("result");
                JsoNaive.JSONValue error = obj.get("error");
                if (result != null) {
                    log("result: " + result);
                } else if (error != null) {
                    log("error: " + error);
                } else {
                    // Malformed
                    log("!! Malformed reply: \"" + reply + "\"");
                }
            } else {
                // Unexpected
                log("!! Malformed reply: \"" + reply + "\"");
            }

        } else {
            // id < 0 signifies a notification (and we will not get a response)

        }
        if (obj != null) {
            return obj;
        } else {
            return Collections.emptyMap();
        }
    }

    /**
     * If reply is an error reply, return its "code" attribute as an integer, if
     * possible, otherwise return 0 (zero).
     *
     * @param reply may be null
     * @return
     */
    private static int errorCode(Map<String, JsoNaive.JSONValue> reply) {
        final int defaultValue = 0;

        if (reply == null) {
            return defaultValue;
        }
        return reply
                //
                .getOrDefault("error", JsoNaive.JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap())
                //
                .getOrDefault("code", JsoNaive.JSONValue.emptyObject())
                //
                .asNumber(defaultValue).intValue();
    }

    /**
     * True when the RPC reply indicates an ordinary failure from Prolog, e.g. after
     * "once" or "call" requests.
     *
     * @param reply may be null
     * @return
     */
    private static boolean isFail(Map<String, JsoNaive.JSONValue> reply) {
        return errorCode(reply) == PROLOG_ERROR_CODE_FAIL;
    }

    /**
     * True when the RPC reply indicates an exception from Prolog, e.g. after "once"
     * or "call" requests.
     *
     * @param reply may be null
     * @return
     */
    private static boolean isException(Map<String, JsoNaive.JSONValue> reply) {
        return errorCode(reply) == PROLOG_ERROR_CODE_EXCEPTION;
    }

    @SuppressWarnings("SameParameterValue")
    private static JsoNaive.JSONValue getExceptionMessage(Map<String, JsoNaive.JSONValue> reply, JsoNaive.JSONValue defaultValue) {
        Map<String, JsoNaive.JSONValue> errorObject = reply
                //
                .getOrDefault("error", JsoNaive.JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap());

        if (errorObject.getOrDefault("code", JsoNaive.JSONValue.emptyObject()).asNumber(0)
                .intValue() == PROLOG_ERROR_CODE_EXCEPTION) {
            return errorObject.getOrDefault("message", defaultValue);
        }

        return defaultValue;
    }

    private static JsoNaive.JSONValue getExceptionData(Map<String, JsoNaive.JSONValue> reply, JsoNaive.JSONValue defaultValue) {
        Map<String, JsoNaive.JSONValue> errorObject = reply
                //
                .getOrDefault("error", JsoNaive.JSONValue.emptyObject())
                //
                .asObject(Collections.emptyMap());

        if (errorObject.getOrDefault("code", JsoNaive.JSONValue.emptyObject()).asNumber(0)
                .intValue() == PROLOG_ERROR_CODE_EXCEPTION) {
            return errorObject.getOrDefault("data", defaultValue);
        }

        return defaultValue;
    }

    /**
     * True when the RPC reply indicates a successful reply from from Prolog, e.g.
     * after "once" or "call" requests.
     *
     * @param reply may be null
     * @return
     */
    private static boolean isSuccess(Map<String, JsoNaive.JSONValue> reply) {
        return reply != null && reply.get("result") != null;
    }

    public static void main(String[] args) throws IOException, InterruptedException {

        if (RUN_TESTS) {
            int failures = 0;
            failures += testJSONParser();
            failures += testJSONWriter();
            if (failures > 0) {
                System.err.println("TEST FAILED: " + failures + " tests failed");
                System.exit(2);
            }
            return;
        }

        String exe_path = (args.length > 0 ? args[0] : "sicstus");
        String code_path = (args.length > 1 ? args[1] : "$SP_LIBRARY_DIR/jsonrpc/jsonrpc_server.pl");

        Process process = launch(exe_path, code_path);

        if (process != null) {
            BufferedReader processOut = new BufferedReader(
                    new InputStreamReader(process.getInputStream(), StandardCharsets.UTF_8));
            Writer processIn = new BufferedWriter(
                    new OutputStreamWriter(process.getOutputStream(), StandardCharsets.UTF_8));
            interact(processIn, processOut);

            process.waitFor();
        } else {
            System.err.println("Could not launch " + exe_path);
            System.exit(1);
        }

    }

    @SuppressWarnings({"UnusedAssignment", "Duplicates"})
    private static void interact(Writer processIn, BufferedReader processOut) throws IOException {
        int id = 0;

        Map<String, JsoNaive.JSONValue> reply;
        Map<String, Object> params;

        // Read the initial state (it is null/Node).
        {
            System.err.print("state ==> ");
            reply = callRPC("state", processIn, processOut, id++);
            System.err.println("State is " + reply.get("result"));
        }

        // Set the state (to 4) and return its previous value
        {
            System.err.print("state:=4 ==> ");
            reply = callRPC("state", Collections.singletonList(4), processIn, processOut, id++);
            System.err.println("State was " + reply.get("result"));
        }
        // Read the current state (it is 4).
        {
            System.err.print("state ==> ");
            reply = callRPC("state", processIn, processOut, id++);
            System.err.println("State is " + reply.get("result"));
        }

        System.err.println();
        // Increment current state by 1.
        {
            String goal = "Result is StateIn+1, StateOut=Result";
            System.err.print("once(" + goal + "). ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal + "."), processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        // Increment current state by 1 (again).
        {
            String goal = "Result is StateIn+1, StateOut=Result";
            System.err.print("once(" + goal + "). ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal + "."), processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println();
        // Increment the state with an increment specified as a VariableName:Value pair
        {
            String goal = "Result is StateIn+Increment, StateOut=Result";
            System.err.print("Increment=5, once(" + goal + "). ==> ");
            params = new LinkedHashMap<>();
            params.put("goal", goal + ".");
            params.put("bindings", Collections.singletonMap("Increment", 5));
            reply = callRPC("once", params, processIn, processOut, id++);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println();
        // Call member(...), backtracking over solutions
        {
            String goal = "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc";
            System.err.print("Multiplier=10, call(" + goal + "). ==> ");

            params = new LinkedHashMap<>();
            params.put("goal", goal + ".");
            params.put("bindings", Collections.singletonMap("Multiplier", 10));
            reply = callRPC("call", params, processIn, processOut, id++);
            System.err.println("First Result=" + reply.get("result"));
        }

        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }

        // Cut, committing to the last solution
        {
            System.err.print("cut ==> ");
            reply = callRPC("cut", processIn, processOut, id++);
            log(reply);
            System.err.println("Result=" + reply.get("result"));
        }

        System.err.println();
        // Backtrack until failure
        {
            String goal = "member(E,[10,20,30]), Inc is Multiplier*E, Result is StateIn+Inc";
            System.err.print("Multiplier=10, " + "call(" + goal + "). ==> ");
            params = new LinkedHashMap<>();
            params.put("goal", goal + ".");
            params.put("bindings", Collections.singletonMap("Multiplier", 10));
            reply = callRPC("call", params, processIn, processOut, id++);
            System.err.println("First Result=" + reply.get("result"));
        }
        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }
        // Ask for the next solution
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);
            System.err.println("(next) Result=" + reply.get("result"));
        }

        // Ask for the next solution (this will fail, since there are only 3 elements in
        // the list)
        {
            System.err.print("retry ==> ");
            reply = callRPC("retry", processIn, processOut, id++);

            if (isSuccess(reply)) {
                System.err.println("(next) Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("Prolog failed (this is expected)");
            } else {
                System.err.println("(next) unepexpected reply:" + reply);
            }
        }

        System.err.println();
        // once(foo is bar). This will throw an exception in Prolog, which translates
        // into an error reply.
        {
            String goal = "foo is bar";
            System.err.print("once(" + goal + "). ==> ");
            reply = callRPC("once", Collections.singletonMap("goal", goal + "."), processIn, processOut, id++);
            if (isSuccess(reply)) {
                System.err.println("Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("Prolog failed");
            } else if (isException(reply)) {
                System.err.println("Prolog threw an exception (this is expected): "
                        + getExceptionData(reply, getExceptionMessage(reply, null)));
            }
        }

        System.err.println();
        // Quit the server.
        {
            System.err.print("quit ==> ");
            reply = callRPC("quit", processIn, processOut, id++);
            if (isSuccess(reply)) {
                System.err.println("Result=" + reply.get("result"));
            } else if (isFail(reply)) {
                System.err.println("Prolog failed");
            } else if (isException(reply)) {
                System.err.println("Prolog threw an exception=" + getExceptionData(reply, getExceptionMessage(reply, null)));
            }
        }
    }

    /** This parser is both slow and incorrect. But it is completely self-contained, short, and suffice for the demo. */
    // The JsoNaive class belong in its own file, but I want this file to be self-contained.
    /*public*/ static class JsoNaive {
        // Matches (some) strings. Group 1 is the value.
        private static final Pattern PATTERN_STRING = Pattern.compile("^\"([^\"\\u0000-\\u0019\\\\]*)\"");
        // Matches (some) numbers. Group 1 is the value.
        private static final Pattern PATTERN_NUMBER = Pattern.compile("^([-+]?(([0])|([1-9][0-9]*))(\\.[0-9]+)?([eE][-+][0-9]+)?)");
        //
        private static final Pattern PATTERN_NULL = Pattern.compile("^null");
        private static final Pattern PATTERN_TRUE = Pattern.compile("^true");
        private static final Pattern PATTERN_FALSE = Pattern.compile("^false");

        public enum CharKind {
            OTHER,
            INVALID,
            EOF,
            WS,
            LEFT_BRACE,
            RIGHT_BRACE,
            LEFT_BRACKET,
            RIGHT_BRACKET,
            COLON,
            DOUBLE_QUOTE,
            DIGIT,
            PLUS,
            N, T, F,
            MINUS,
            BACKSLASH
        }

        public static abstract class JSONValue {
            @SuppressWarnings("WeakerAccess")
            public static JSONValue emptyObject() {
                return new JSONObject(Collections.emptyMap());
            }

            /**
             * The {@linkplain #type() type of} a JSON value.
             */
            public enum Type {
                String,
                Number,
                Object,
                Array,
                Null,
                True,
                False
            }

            JSONValue() {

            }

            /**
             * Returns the string value of the JSON value, if any. String values are available for non-compound values,
             * i.e. strings, numbers, and the special constants null, true, and false.
             *
             * @return the string value, if any. Otherwise null.
             */
            @SuppressWarnings("unused")
            public abstract String value();

            /**
             * If the receiver represents a JSON object, returns a map. Otherwise returns null.
             *
             * @return the map, or null if none.
             */
            @SuppressWarnings("unused")
            public Map<String, JSONValue> map() {
                return null;
            }

            /**
             * If the receiver represents a JSON array, returns a list of its elements. Otherwise returns null.
             *
             * @return the list of elements, or null if none.
             */
            @SuppressWarnings("unused")
            public List<JSONValue> array() {
                return null;
            }

            /**
             * Returns the {@linkplain Type type} of the receiver.
             *
             * @return The {@linkplain Type type}.
             */
            @SuppressWarnings("unused")
            public abstract Type type();


            @SuppressWarnings("WeakerAccess")
            public Map<String, JSONValue> asObject(Map<String, JSONValue> defaultValue) {
                if (type() == JSONValue.Type.Object) {
                    return map();
                }
                return defaultValue;
            }

            @SuppressWarnings("WeakerAccess")
            public Map<String, JSONValue> asObject() {
                return asObject(null);
            }

            @SuppressWarnings("WeakerAccess")
            public JSONValue[] asArray(JSONValue[] defaultValue) {
                if (type() == JSONValue.Type.Array) {
                    array().toArray(new JSONValue[0]);
                }
                return defaultValue;
            }

            @SuppressWarnings("WeakerAccess")
            public JSONValue[] asArray() {
                return asArray(null);
            }

            @SuppressWarnings("WeakerAccess")
            public String asString(String defaultValue) {
                if (type() == JSONValue.Type.String) {
                    return value();
                }
                return defaultValue;
            }

            @SuppressWarnings("WeakerAccess")
            public String asString() {
                return asString(null);
            }

            @SuppressWarnings("WeakerAccess")
            public Number asNumber(Number defaultValue) {
                if (type() == JSONValue.Type.Number) {
                    String v = value();
                    if (v.indexOf('.') > 0 || v.indexOf('e') > 0 || v.indexOf('E') > 0) {
                        return Double.valueOf(v);
                    } else {
                        return new BigInteger(v, 10);
                    }
                }
                return defaultValue;
            }

            @SuppressWarnings("WeakerAccess")
            public Number asNumber() {
                return asNumber(null);
            }


            @SuppressWarnings("WeakerAccess")
            public Boolean asBoolean(Boolean defaultValue) {
                if (type() == JSONValue.Type.True) {
                    return Boolean.TRUE;
                }
                if (type() == JSONValue.Type.False) {
                    return Boolean.FALSE;
                }
                return defaultValue;
            }

            @SuppressWarnings("WeakerAccess")
            public Boolean asBoolean() {
                return asBoolean(null);
            }

            @SuppressWarnings("WeakerAccess")
            public boolean isNull() {
                return type() == JSONValue.Type.Null;
            }

        }

        public static class JSONStringValue extends JSONValue {
            private final String iString;

            JSONStringValue(String string) {
                iString = Objects.requireNonNull(string);
            }

            @Override
            public String toString() {
                return '"' + value() + '"';
            }

            @Override
            public String value() {
                return iString;
            }

            @Override
            public Type type() {
                return Type.String;
            }
        }

        public static class JSONNumberValue extends JSONValue {
            private final String iString;

            JSONNumberValue(String string) {
                iString = Objects.requireNonNull(string);
            }

            @Override
            public String toString() {
                return value();
            }

            @Override
            public String value() {
                return iString;
            }

            @Override
            public Type type() {
                return Type.Number;
            }
        }

        public static class JSONNullValue extends JSONValue {
            JSONNullValue() {
            }

            @Override
            public String value() {
                return "null";
            }

            @Override
            public String toString() {
                return value();
            }

            @Override
            public Type type() {
                return Type.Null;
            }
        }

        public static class JSONTrueValue extends JSONValue {
            JSONTrueValue() {
            }

            @Override
            public String value() {
                return "true";
            }

            @Override
            public String toString() {
                return value();
            }

            @Override
            public Type type() {
                return Type.True;
            }
        }

        public static class JSONFalseValue extends JSONValue {
            JSONFalseValue() {
            }

            @Override
            public String value() {
                return "false";
            }

            @Override
            public String toString() {
                return value();
            }

            @Override
            public Type type() {
                return Type.False;
            }
        }

        public static class JSONObject extends JSONValue {
            final Map<String, JSONValue> iMap;

            JSONObject(Map<String, JSONValue> map) {
                this.iMap = Objects.requireNonNull(map);
            }

            @Override
            public String toString() {
                StringBuilder sb = new StringBuilder();
                String prefix = "";

                sb.append('{');
                for (Map.Entry<String, JSONValue> e : iMap.entrySet()) {
                    sb.append(prefix);
                    sb.append('"').append(e.getKey()).append('"');
                    sb.append(": ");
                    sb.append(e.getValue().toString());
                    prefix = ", ";
                }
                sb.append('}');
                return sb.toString();
            }

            @Override
            public String value() {
                return null;
            }

            public Map<String, JSONValue> map() {
                return iMap;
            }

            @Override
            public Type type() {
                return Type.Object;
            }
        }

        public static class JSONList extends JSONValue {
            final List<JSONValue> iElements;

            JSONList(List<JSONValue> elements) {
                this.iElements = Objects.requireNonNull(elements);
            }

            @Override
            public String toString() {
                StringBuilder sb = new StringBuilder();
                String prefix = "";

                sb.append('[');
                for (JSONValue e : iElements) {
                    sb.append(prefix).append(e.toString());
                    prefix = ", ";
                }
                sb.append(']');
                return sb.toString();
            }

            @Override
            public String value() {
                return null;
            }

            @Override
            public List<JSONValue> array() {
                return iElements;
            }

            @Override
            public Type type() {
                return Type.Array;
            }
        }

        private static final int EOF = -1;

        private final String iString;
        private final int iPosExclusiveLimit;
        private int iPos;

        @SuppressWarnings("WeakerAccess")
        public JsoNaive(String string) {
            iString = string;
            iPosExclusiveLimit = string.length();
            iPos = 0;
        }

        /**
         * Skip whitespace and return the character (or {@link #EOF}) that comes after.
         *
         * @return the peeked character after any whitespace has been skipped.
         */
        private int skipWS() {
            int c;
            while (classifyChar(c = peek()) == CharKind.WS) {
                skip();
            }
            return c;
        }

        private CharKind classifyChar(int c) {
            switch (c) {
                case EOF:
                    return CharKind.EOF;
                case '{':
                    return CharKind.LEFT_BRACE;
                case '}':
                    return CharKind.RIGHT_BRACE;
                case '[':
                    return CharKind.LEFT_BRACKET;
                case ']':
                    return CharKind.RIGHT_BRACKET;
                case ':':
                    return CharKind.COLON;
                case '"':
                    return CharKind.DOUBLE_QUOTE;
                case 0x20: // Space
                case 0x09:// Horizontal tab
                case 0x0A: // Line feed or New line
                case 0x0D: // Carriage return
                    return CharKind.WS;
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    return CharKind.DIGIT;
                case '+':
                    return CharKind.PLUS;
                case '-':
                    return CharKind.MINUS;
                case 'n':
                    return CharKind.N;
                case 't':
                    return CharKind.T;
                case 'f':
                    return CharKind.F;
                case '\\':
                    return CharKind.BACKSLASH;
                default:
                    if (c < 0x20) {
                        return CharKind.INVALID;
                    }
                    return CharKind.OTHER;
            }
        }

        @SuppressWarnings({"DuplicateBranchesInSwitch", "WeakerAccess"})
        public JSONValue parseValue() {
            switch (nextTokenKind()) {
                case OTHER:
                    break;
                case INVALID:
                    break;
                case EOF:
                    return null;
                case WS:
                    assert false : "Impossible case";
                    break;
                case LEFT_BRACE:
                    return parseObject();
                case RIGHT_BRACE:
                    return null;
                case LEFT_BRACKET:
                    return parseArray();
                case RIGHT_BRACKET:
                    break;
                case COLON:
                    break;
                case DOUBLE_QUOTE:
                    return parseString();
                case DIGIT:
                case PLUS:
                case MINUS:
                    return parseNumber();
                case N:
                    return parseNull();
                case T:
                    return parseTrue();
                case F:
                    return parseFalse();

                case BACKSLASH:
                    break;
                default:
                    assert false : "Impossible case";
                    break;
            }
            // Parse error.
            return null;
        }

        private JSONValue parseNull() {
            Matcher matcher = PATTERN_NULL.matcher(iString);

            if (startsWithMatch(matcher)) {
                iPos = matcher.end();
                return new JSONNullValue();
            }
            return null;
        }

        private boolean startsWithMatch(Matcher matcher) {
            matcher.region(iPos, iPosExclusiveLimit);
            matcher.useAnchoringBounds(true);
            return matcher.lookingAt();
        }

        private JSONValue parseTrue() {
            Matcher matcher = PATTERN_TRUE.matcher(iString);

            if (startsWithMatch(matcher)) {
                iPos = matcher.end();
                return new JSONTrueValue();
            }
            return null;
        }

        private JSONValue parseFalse() {
            Matcher matcher = PATTERN_FALSE.matcher(iString);

            if (startsWithMatch(matcher)) {
                iPos = matcher.end();
                return new JSONFalseValue();
            }
            return null;
        }

        private JSONNumberValue parseNumber() {
            Matcher matcher = PATTERN_NUMBER.matcher(iString);

            if (startsWithMatch(matcher)) {
                iPos = matcher.end();
                return new JSONNumberValue(matcher.group(1));
            }
            return null;
        }

        private JSONStringValue parseString() {
            Matcher matcher = PATTERN_STRING.matcher(iString);

            if (startsWithMatch(matcher)) {
                iPos = matcher.end();
                return new JSONStringValue(matcher.group(1));
            }
            return null;
        }

        private JSONObject parseObject() {
            Map<String, JSONValue> map = new LinkedHashMap<>();

            skip('{');

            if (skipWS() != '}') {
                // Any number of ( key:value comma ) followed by key:value
                while (true) {
                    JSONStringValue key = parseString();
                    if (key == null) {
                        return null;
                    }

                    if (skipWS() != ':') {
                        return null;
                    }
                    skip(':');

                    JSONValue value = parseValue();
                    if (value == null) {
                        return null;
                    }

                    map.put(key.value(), value);

                    if (skipWS() != ',') {
                        break;
                    }
                    skip(',');
                    skipWS();
                }
            }

            if (peek() == '}') {
                skip('}');
                return new JSONObject(map);
            }

            return null;
        }

        private JSONValue parseArray() {
            List<JSONValue> elements = new ArrayList<>();

            skip('[');

            if (skipWS() != ']') {
                // Any number of ( value comma ) followed by value
                while (true) {
                    JSONValue value = parseValue();
                    if (value == null) {
                        return null;
                    }
                    elements.add(value);
                    if (skipWS() != ',') {
                        break;
                    }
                    skip(',');
                    skipWS();
                }
            }

            if (skipWS() == ']') {
                skip(']');
                return new JSONList(elements);
            }

            return null;
        }


        private CharKind nextTokenKind() {
            skipWS();

            return classifyChar(peek());
        }

        private int next() {
            if (iPos < iPosExclusiveLimit) {
                return iString.charAt(iPos++);
            }
            return EOF;
        }

        private int peek() {
            if (iPos < iPosExclusiveLimit) {
                return iString.charAt(iPos);
            }
            return EOF;
        }

        private void skip() {
            next();
        }

        private void skip(int expect) {
            int c = next();
            assert c == expect;
        }


        public static void main(String[] args) {

            for (String s : Arrays.asList("  \"abc\" ", "  12.34 ", "0e0", "12.3E+5", "  null", "  true", "  false", "  null ", "  true ", "  false ",
                    " []", "[ \"a\", 42, null ] ",
                    "{}", " { \"a\":  44 , \"abc\"  :true}"
            )) {
                JSONValue v = new JsoNaive(s).parseValue();
                System.err.format("Parsed: \"%s\" ==> %s\n", s, v);
            }
        }
    }

}
