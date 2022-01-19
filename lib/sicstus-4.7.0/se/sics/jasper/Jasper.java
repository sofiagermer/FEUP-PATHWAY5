/*
 * Copyright (C) 2018, RISE Research Institutes of Sweden AB.
 */

package se.sics.jasper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Stack;

/**
 * This class provides the multi-threaded functionality of the Java-Prolog
 * interface. The User's Manual documents how to use this class.
 *
 * <p>
 * The interfaces {@link Prolog}, {@link Query} and {@link Term} declare the
 * methods available to the Java programmer for access to SICStus runtime.
 *
 */
public class Jasper {
    // *** FIX? remove the counters when non-Beta. Or at least remove most of the
    // usage.
    private final Counter tokencounter;
    private final Counter queuecounter;
    private final JasperProlog jasperProlog;
    private final PrologServer jasperServer;

    private static final int jasperDebug = Integer.getInteger("debugLevel", 0).intValue();

    final boolean debugging() {
        return debugging(1);
    }

    final boolean debugging(int level) {
        return jasperDebug >= level;
    }

    static void debugPrintln(String msg) {
        System.err.println("Thread " + Thread.currentThread().getName() + ": " + msg);
        System.err.flush();
    }

    static void debugPrint(String msg) {
        System.err.print("Thread " + Thread.currentThread().getName() + ": " + msg);
        System.err.flush();
    }

    static class Counter {
        private int c = 0;

        public int next() {
            return c += 1;
        }
    }

    class TokenQueue {
        private final String name;
        private final Stack<Token> qstack = new Stack<>();

        synchronized public void put(Token t) {
            if (debugging(5))
                debugPrintln("Entering TokenQueue.put()");
            if (debugging(5))
                debugPrintln("    this queue: " + this);
            boolean wasEmpty = isEmpty();
            if (debugging(5))
                debugPrintln("    wasEmpty==" + wasEmpty);
            if (debugging(5))
                debugPrintln("    putting token: " + t);
            t.debugPrintToken(10);
            qstack.push(t);
            if (debugging(10))
                debugPrintln("    queue size: " + qstack.size());
            if (wasEmpty) {
                notifyAll();
            }
        }

        synchronized public Token get() throws InterruptedException {
            if (debugging(5))
                debugPrintln("Entering TokenQueue.get()");
            if (debugging(5))
                debugPrintln("    this queue: " + this);
            waitfor();
            if (debugging(5))
                debugPrintln("in TokenQueue.get(), after waitfor()");
            if (debugging(5))
                debugPrintln("    this queue:" + this);
            if (debugging(10))
                debugPrintln("    queue size: " + qstack.size());
            Token tok = qstack.pop();
            if (debugging(5))
                debugPrintln("    getting token: " + tok);
            tok.debugPrintToken(10);
            return tok;
        }

        synchronized public boolean isEmpty() {
            return qstack.empty();
        }

        synchronized public void waitfor() throws InterruptedException {
            if (debugging(5))
                debugPrintln("Entering TokenQueue.waitfor()");
            if (debugging(5))
                debugPrintln("    this queue: " + this);
            if (debugging(5))
                debugPrintln("    isEmpty():  " + isEmpty());
            while (isEmpty()) {
                wait();
            }
            if (debugging(5))
                debugPrintln("Exiting TokenQueue.waitfor()");
        }

        @Override
        public String toString() {
            return this.name;
        }

        TokenQueue(String name) {
            this.name = "TokenQueue-" + queuecounter.next() + "-" + name;
            if (debugging(5))
                debugPrintln("Creating TokenQueue(): " + this.name);
        }
    }

    class Token {
        private final String name;
        private final TokenQueue myqueue; // Return address.
        private final Object obj;
        private final String methodname;
        private final Class<?>[] typesig;
        private final Object[] args;
        private final boolean staticP; // static or instance
        private boolean hasResult;
        private Object result;
        private Exception excp;

        boolean isStopServerMessage() {
            return false;
        }

        TokenQueue getQueue() {
            return myqueue;
        }

        Object getObject() {
            return obj;
        }

        String getMethodname() {
            return methodname;
        }

        Class<?>[] getTypesig() {
            return typesig;
        }

        Object[] getArgs() {
            return args;
        }

        boolean getStaticP() {
            return staticP;
        }

        void setResult(Object res) {
            this.result = res;
            hasResult = true;
        }

        Object getResult() {
            return result;
        }

        void setException(Exception ex) {
            this.excp = ex;
        }

        boolean checkException() {
            return (excp != null);
        }

        Exception getException() {
            return excp;
        }

        boolean checkResult() {
            return hasResult;
        }

        @Override
        public String toString() {
            return this.name;
        }

        void debugPrintToken(int debugLevel) {
            if (debugging(debugLevel)) {
                debugPrintln("  *Token*:");
                debugPrintln("    token:      " + name);
                debugPrintln("    myqueue:    " + myqueue);
                debugPrintln("    obj.getClass().getName():  " + obj.getClass().getName());
                debugPrintln("    obj.hashCode(): " + obj.hashCode());
                debugPrintln("    methodname: " + methodname);
                debugPrintln("    typesig:    " + Arrays.toString(typesig));
                debugPrintln("    staticP:    " + staticP);
                debugPrintln("    result is " + (result == null ? "null" : result.getClass().getName()));
                debugPrintln("    excp is " + (excp == null ? "null" : "not null"));
            }
        }

        Token(String name, TokenQueue tq, Object obj, String methodname, Object[] args, Class<?>[] typesig,
                boolean staticP) {
            this.name = "Token-" + tokencounter.next() + "-" + name;
            this.myqueue = tq;
            this.obj = obj;
            this.methodname = methodname;
            this.args = args;
            this.typesig = typesig;
            this.staticP = staticP;
            this.result = null;
            this.hasResult = false;
            this.excp = null;
        }
    }

    class NewObjectToken extends Token {
        NewObjectToken(String name, TokenQueue tq, Object obj, String methodname, Object[] args, Class<?>[] typesig) {
            super(name, tq, obj, methodname, args, typesig, true);
        }
    }

    class StopToken extends Token {
        @Override
        boolean isStopServerMessage() {
            return true;
        }

        @Override
        void debugPrintToken(int debugLevel) {
            if (debugging(debugLevel))
                debugPrintln("  *StopToken*");
        }

        StopToken() {
            super(null, null, null, null, null, null, false);
        }
    }

    class PrologServer implements Runnable, Server {
        private String[] argv;
        private String savFile;
        private SICStus sp = null;
        private TokenQueue serverqueue = null;
        private TokenQueue clientqueue = null;
        private boolean runFlag = true;

        void initPrologServer() {
            serverqueue = new TokenQueue("Server");
            clientqueue = new TokenQueue("Client");
            runFlag = true;
        }

        PrologServer(String[] argv, String savFile) {
            this.argv = argv;
            this.savFile = savFile;
            sp = null;
            initPrologServer();
        }

        PrologServer(SICStus sp) {
            setSICStus(sp);
            initPrologServer();
        }

        @Override
        public void run() {
            if (sp == null) {
                SICStus sp;
                try {
                    if (debugging(10))
                        debugPrintln("in PrologServer.run(): trying to create sp");
                    sp = new SICStus(argv);
                    if (debugging(10))
                        debugPrintln("in PrologServer.run(): sp created");
                    if (savFile != null) {
                        sp.restore(savFile);
                    }
                } catch (Throwable th) {
                    setNoSICStus();
                    throw new Error("Cannot create a SICStus object. " + th);
                }
                setSICStus(sp);
            }
            if (!sp.spSetThreadServerMode(true)) {
                throw new Error("Cannot set thread server mode.");
            }
            TokenQueue srvq = getServerqueue();
            try {
                while (runFlag) {
                    if (debugging(5))
                        debugPrintln("in PrologServer.run(); runFlag-loop");
                    Token message = srvq.get();
                    boolean stop = message.isStopServerMessage();
                    if (stop)
                        break;
                    while (!message.checkResult()) {
                        if (debugging(10))
                            debugPrintln("    message:");
                        message.debugPrintToken(10);
                        callMethod(sp, message);
                        clientqueue.put(message);
                        message = srvq.get();
                        stop = message.isStopServerMessage();
                        if (stop)
                            break;
                    }
                    if (stop)
                        break;
                }
            } catch (Exception ex) {
                if (debugging(10))
                    debugPrint("Exception in PrologServer.run():");
                ex.printStackTrace(System.err);
            } finally {
                sp.spSetThreadServerMode(false);
            }
        }

        @Override
        public void stopServer() {
            if (runFlag) {
                TokenQueue srvq = getServerqueue();
                Token stopToken = new StopToken();
                runFlag = false;
                srvq.put(stopToken);
            }
        }

        synchronized void setSICStus(SICStus sp) {
            this.sp = sp;
            sp.setServer(this);
            notifyAll();
        }

        synchronized private void setNoSICStus() {
            notifyAll();
        }

        @Override
        synchronized public SICStus getSICStus() throws InterruptedException {
            while (sp == null) {
                wait();
            }
            return sp;
        }

        TokenQueue getServerqueue() {
            return serverqueue;
        }

        TokenQueue getClientqueue() {
            return clientqueue;
        }

        class TypeSigConversionException extends Exception {
            private static final long serialVersionUID = 1L;

            TypeSigConversionException(String str, int i) {
                super(str + " " + i);
            }
        }

        ClassLoader ourClassLoader = null;

        Class<?> forName(String name) throws ClassNotFoundException {
            if (ourClassLoader == null) {
                ourClassLoader = this.getClass().getClassLoader();
            }
            return Class.forName(name, false, ourClassLoader);
        }

        // Convert a JNI-style type signature, a String, to a type signature
        // acceptable for Method.invoke(), a Class<?>[].
        //
        Class<?>[] convertTypesig(String typeSigString) throws TypeSigConversionException, ClassNotFoundException {
            ArrayList<Class<?>> al = new ArrayList<>();
            String argTypeString = typeSigString.substring(typeSigString.indexOf("(") + 1, typeSigString.indexOf(")"))
                    .replace('/', '.');
            for (int i = 0; i < argTypeString.length(); i++) {
                int arrayCount = 0;
                char tc = argTypeString.charAt(i);
                Class<?> cc = null;
                switch (tc) {
                case '[':
                    arrayCount++;
                    char ac = argTypeString.charAt(++i);
                    while (ac == '[') {
                        arrayCount++;
                        ac = argTypeString.charAt(++i);
                    }
                    char[] arrayPrefix = new char[arrayCount];
                    Arrays.fill(arrayPrefix, '[');
                    switch (ac) {
                    case 'L':
                        int ii = argTypeString.indexOf(";", i + 1);
                        cc = forName(new String(arrayPrefix) + argTypeString.substring(i, ii + 1));
                        i = ii;
                        break;
                    case 'Z':
                    case 'B':
                    case 'C':
                    case 'S':
                    case 'I':
                    case 'J':
                    case 'F':
                    case 'D':
                        cc = forName(new String(arrayPrefix) + argTypeString.substring(i, i + 1));
                        break;
                    default:
                        throw new TypeSigConversionException(argTypeString, i);
                    }
                    break;
                case 'L':
                    int ii = argTypeString.indexOf(";", i + 1);
                    cc = forName(argTypeString.substring(i + 1, ii));
                    i = ii;
                    break;
                case 'Z':
                    cc = boolean.class;
                    break;
                case 'B':
                    cc = byte.class;
                    break;
                case 'C':
                    cc = char.class;
                    break;
                case 'S':
                    cc = short.class;
                    break;
                case 'I':
                    cc = int.class;
                    break;
                case 'J':
                    cc = long.class;
                    break;
                case 'F':
                    cc = float.class;
                    break;
                case 'D':
                    cc = double.class;
                    break;
                default:
                    throw new TypeSigConversionException(argTypeString, i);
                }
                if (cc == null) {
                    throw new TypeSigConversionException(argTypeString, i);
                }
                al.add(cc);
            }
            return al.toArray(new Class<?>[0]);
        }

        // The callBack<Foo> methods are called from jasper.c
        // *NOTE*: They are therefore called in the server thread!
        // When they are called the PrologClient is waiting for
        // the PrologServer to service a request. The client
        // checks for callbacks, executes one or more callbacks
        // and then continues to wait for a notification from the
        // server.

        public Token callBack(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Class<?>[] ctypesig = convertTypesig(typesig);
            if (debugging(5))
                debugPrintln("Entering PrologServer.callBack()");
            Token cbtok = new Token("Server", serverqueue, obj, methname, args, ctypesig, staticP);
            return callBackLoop(cbtok);

        }

        Token callBackLoop(Token cbtok) throws InterruptedException, Exception {
            clientqueue.put(cbtok);
            cbtok = serverqueue.get();
            while (!cbtok.checkResult()) {
                callMethod(sp, cbtok);
                clientqueue.put(cbtok);
                cbtok = serverqueue.get();
            }
            // cbtok now has a return value, and possibly an exception value.
            // Same as in PrologClient.sendMessage(): throw the exception. It will be
            // caught by callMethod() and propagated further.
            // [PD] 3.9.1. No, no! That will break things. We must not throw an exception
            // here, since that will disrupt the calling sequence in the server
            // thread. Or maybe not? Throwing the exception is the only way to propagate it,
            // isn't it?
            // Simply pass the token to the client, and let it handle the exception.
            // *** FIX? We should perhaps not propagate InterruptedException?

            if (cbtok.checkException()) {
                Exception ex = cbtok.getException();
                throw (Exception) ex.fillInStackTrace();
            }

            return cbtok;
        }

        public Object callBackNewObject(Object obj, String methname, Object[] args, String typesig)
                throws InterruptedException, Exception {
            Class<?>[] ctypesig = convertTypesig(typesig);
            Token cbtok = new NewObjectToken("Server", serverqueue, obj, methname, args, ctypesig);
            Token cbtok2 = callBackLoop(cbtok);
            return cbtok2.getResult();
        }

        public void callBackVoid(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering PrologServer.callBackVoid()");
            callBack(obj, methname, args, typesig, staticP);
        }

        public Object callBackObject(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering PrologServer.callBackObject()");
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return cbtok.getResult();
        }

        public boolean callBackBoolean(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering PrologServer.callbackBoolean()");
            if (debugging(10))
                debugPrintln("    obj:      " + obj);
            if (debugging(10))
                debugPrintln("    methname: " + methname);
            if (debugging(10))
                debugPrintln("    args:     " + args);
            if (debugging(10))
                debugPrintln("    typesig:  " + typesig);
            if (debugging(10)) {
                debugPrintln("x" + methname + "y");
                if (methname.equals("barbool")) {
                    debugPrintln("[" + methname + "]");
                    debugPrintln("    args[6]==" + args[6]);
                }
            }
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Boolean) cbtok.getResult()).booleanValue();
        }

        public byte callBackByte(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Byte) cbtok.getResult()).byteValue();
        }

        public char callBackChar(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Character) cbtok.getResult()).charValue();
        }

        public short callBackShort(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Short) cbtok.getResult()).shortValue();
        }

        public int callBackInt(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering PrologServer.callBackInt()");
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            if (debugging(10))
                debugPrintln("in PrologServer.callBackInt(), after callBack()");
            if (debugging(10))
                debugPrintln("    cbtok.getResult()==" + cbtok.getResult());
            if (debugging(10))
                debugPrintln("    intvalue: " + ((Integer) cbtok.getResult()).intValue());
            return ((Integer) cbtok.getResult()).intValue();
        }

        public long callBackLong(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering PrologServer.callbackLong()");
            if (debugging(10))
                debugPrintln("    obj:      " + obj);
            if (debugging(10))
                debugPrintln("    methname: " + methname);
            if (debugging(10))
                debugPrintln("    args:     " + args);
            if (debugging(10))
                debugPrintln("    typesig:  " + typesig);
            if (debugging(10))
                debugPrintln("    staticP:  " + staticP);
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            cbtok.debugPrintToken(2);
            return ((Long) cbtok.getResult()).longValue();
        }

        public float callBackFloat(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Float) cbtok.getResult()).floatValue();
        }

        public double callBackDouble(Object obj, String methname, Object[] args, String typesig, boolean staticP)
                throws InterruptedException, Exception {
            Token cbtok = callBack(obj, methname, args, typesig, staticP);
            return ((Double) cbtok.getResult()).doubleValue();
        }

        // called by "glue" (i.e. spnative), in the server thread.
        JasperTerm newGlueJasperTerm(SPTerm spt) throws InterruptedException, Exception {
            if (debugging())
                debugPrintln("Entering newGlueJasperTerm()");
            if (debugging()) {
                debugPrintln("in newGlueJasperTerm() (1)");
                debugPrintln("    spt==" + spt);
                debugPrintln("    spt.hashCode()==" + spt.hashCode());
            }
            Object obj[] = new Object[] { spt };
            if (debugging())
                debugPrintln("in newGlueJasperTerm() (2)");
            return (JasperTerm) callBackObject(this, "newGlueJasperTermFromClient", obj,
                    "(Lse/sics/jasper/SPTerm;)Lse/sics/jasper/Jasper$JasperTerm;", false);
        }

        // Must be called from the client thread!
        // [PD] 4.0.5 This has to be public.
        public JasperTerm newGlueJasperTermFromClient(SPTerm spt) throws InterruptedException // , Exception
        {
            return new JasperTerm(jasperProlog, this, spt);
        }
    }

    class PrologClient {
        final PrologServer myServer;
        SICStus sp = null;
        private final TokenQueue serverqueue;
        final TokenQueue clientqueue;

        public Server getServer() {
            return myServer;
        }

        void pushCaller(Prolog caller) {
            JasperGlobalCallerStack.get().push(caller);
        }

        void popCaller() {
            JasperGlobalCallerStack.get().pop();
        }

        synchronized Token sendMessage(PrologClient owner, Class<?>[] typesig, Object obj, String methodName,
                Object[] args) throws Exception {

            Token reply;
            Token request = new Token(Thread.currentThread().getName(), clientqueue, obj, methodName, args, typesig,
                    false);
            serverqueue.put(request);
            reply = clientqueue.get();
            while (!reply.checkResult()) {
                if (debugging(5))
                    debugPrintln("In sendMessage(), owner==" + owner);
                pushCaller((Prolog) owner);
                callMethod(sp, reply);
                popCaller();
                serverqueue.put(reply);
                reply = clientqueue.get();
            }
            // *** FIX? We should perhaps not propagate InterruptedException?
            if (debugging(5))
                debugPrintln("In sendMessage(), checking for exception (this is expected)");
            reply.debugPrintToken(5);
            if (reply.checkException()) {
                Exception ex = reply.getException();
                throw (Exception) ex.fillInStackTrace();
            }
            return reply;
        }

        // *** FIX? Consider making this private.
        // [PD] 3.9.1 SPRM 3305
        Token sendMessage(Class<?>[] typesig, Object obj, String methodName, Object[] args) throws Exception {
            if (debugging(5))
                debugPrintln("Entering sendMessage(), this.getClass()==" + this.getClass());
            return sendMessage(this, typesig, obj, methodName, args);
        }

        PrologClient(PrologServer server) {
            if (debugging())
                debugPrintln("Creating a PrologClient from thread " + Thread.currentThread());
            this.myServer = server;
            serverqueue = myServer.getServerqueue();
            clientqueue = myServer.getClientqueue();
        }

        PrologClient(PrologServer server, SICStus sp) {
            /*
             * initQueues(server); initialThread = Thread.currentThread();
             */
            this(server);
            this.sp = sp;
        }
    }

    class JasperProlog extends PrologClient implements Prolog {
        @Override
        public Query openPrologQuery(String string, Map<String, ? extends Term> varMap) throws Exception {
            if (debugging(5))
                debugPrintln("entering JasperProlog.openPrologQuery()");

            // Cast it to the type we would have liked to use in the parameter list.
            @SuppressWarnings("unchecked")
            Map<String, Term> varTermMap = (Map<String, Term>) varMap;

            return new JasperQuery(this, myServer, string, varTermMap);
        }

        private boolean queryHelper(String methodName, String string, Map<String, Term> varMap) throws Exception {
            if (debugging(5))
                debugPrintln("entering JasperProlog.queryHelper()");
            if (debugging(10))
                debugPrintln("    string==" + string);
            if (varMap != null) {
                // Unwrap all SPTerms
                Iterator<Entry<String, Term>> it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Entry<String, Term> me = it.next();
                    Object val = me.getValue();
                    if (debugging(10))
                        debugPrintln("    val.getClass()==" + val.getClass());
                    if (val instanceof JasperTerm) {
                        me.setValue(((JasperTerm) val).getSPTerm());
                    }
                }
                if (debugging(10))
                    debugPrintln("JasperProlog.queryHelper(), after unwrap");
            }
            Token request = this.sendMessage(new Class<?>[] { String.class, Map.class }, sp, methodName,
                    new Object[] { string, varMap });
            if (varMap != null) {
                // Wrap all SPTerms in JasperTerm objects
                Iterator<Entry<String, Term>> it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Entry<String, Term> me = it.next();
                    me.setValue(new JasperTerm(this, myServer, (SPTerm) me.getValue()));
                }
            }
            if (debugging(10))
                debugPrintln("JasperProlog.queryHelper(), after wrap");
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean query(String string, Map<String, ? extends Term> varMap) throws Exception {
            if (debugging(5))
                debugPrintln("entering JasperProlog.prologQuery()");

            // Cast it to the type we would have liked to use in the parameter list.
            @SuppressWarnings("unchecked")
            Map<String, Term> varTermMap = (Map<String, Term>) varMap;

            return queryHelper("query", string, varTermMap);
        }

        @Override
        public boolean queryCutFail(String string, Map<String, ? extends Term> varMap) throws Exception {
            if (debugging(5))
                debugPrintln("entering JasperProlog.prologQueryCutFail()");

            // Cast it to the type we would have liked to use in the parameter list.
            @SuppressWarnings("unchecked")
            Map<String, Term> varTermMap = (Map<String, Term>) varMap;

            return queryHelper("queryCutFail", string, varTermMap);
        }

        @Override
        public Term newTerm() throws InterruptedException, Exception {
            if (debugging(5))
                debugPrintln("Entering JasperProlog.newTerm()");
            Token request = this.sendMessage(new Class<?>[] {}, sp, "newTerm", new Object[] {});
            SPTerm spt = (SPTerm) request.getResult();
            // [PD] 3.11.0 SPTerm.toString() cannot be called from the client thread
            // if (debugging(5)) debugPrintln("in JasperProlog.newTerm(), spt==" +
            // spt.toString());
            if (debugging(5))
                debugPrintln("in JasperProlog.newTerm(), spt==" + spt.superToString());
            if (debugging(5))
                debugPrintln("in JasperProlog.newTerm(), spt.isValid()==" + spt.isValid());
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(Term t) throws InterruptedException, Exception {
            Token request = this.sendMessage(new Class<?>[] { Term.class }, sp, "newTerm",
                    new Object[] { ((JasperTerm) t).getSPTerm() });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(int i) throws InterruptedException, Exception // integer
        {
            Token request = this.sendMessage(new Class<?>[] { Integer.class }, sp, "newTerm",
                    new Object[] { Integer.valueOf(i) });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(long j) throws InterruptedException, Exception // integer
        {
            Token request = this.sendMessage(new Class<?>[] { Long.class }, sp, "newTerm",
                    new Object[] { Long.valueOf(j) });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(double d) throws InterruptedException, Exception // float
        {
            Token request = this.sendMessage(new Class<?>[] { Double.class }, sp, "newTerm",
                    new Object[] { Double.valueOf(d) });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(float f) throws InterruptedException, Exception // float
        {
            Token request = this.sendMessage(new Class<?>[] { Float.class }, sp, "newTerm",
                    new Object[] { Float.valueOf(f) });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(String a) throws InterruptedException, Exception // atom
        {
            Token request = this.sendMessage(new Class<?>[] { String.class }, sp, "newTerm", new Object[] { a });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newTerm(String functor, Term args[]) throws InterruptedException, Exception // functor
        {
            if (debugging(5))
                debugPrintln("Entering JasperProlog.newTerm(String,Term[])");
            // *** FIX? Is `new SPTerm[]' OK?
            SPTerm[] sptargs = new SPTerm[args.length];
            if (debugging(10))
                debugPrintln("in JasperProlog.newTerm(String,Term[]), sptargs created");
            for (int i = 0; i < args.length; i++) {
                if (debugging(10))
                    debugPrintln("in JasperProlog.newTerm(String,Term[]), i==" + i);
                sptargs[i] = ((JasperTerm) args[i]).getSPTerm();
            }
            if (debugging(10))
                debugPrintln("in JasperProlog.newTerm(String,Term[]), before sendMessage");
            Token request = this.sendMessage(new Class<?>[] { String.class, Term[].class }, sp, "newTerm",
                    new Object[] { functor, sptargs });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term prologReadFromString(String string, Map<String, ? extends Term> varMap)
                throws InterruptedException, Exception {
            Token request = sendMessage(new Class<?>[] { String.class, Map.class }, sp, "prologReadFromString",
                    new Object[] { string, varMap });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newVariable() throws InterruptedException, Exception {
            Token request = sendMessage(new Class<?>[] {}, sp, "newVariable", new Object[] {});
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term consFunctor(String functor, Term[] args) throws InterruptedException, Exception {
            SPTerm[] sptargs = new SPTerm[args.length];
            for (int i = 0; i < args.length; i++) {
                sptargs[i] = ((JasperTerm) args[i]).getSPTerm();
            }
            Token request = sendMessage(new Class<?>[] { String.class, Term[].class }, sp, "consFunctor",
                    new Object[] { functor, sptargs });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term consList(Term head, Term tail) throws InterruptedException, Exception {
            SPTerm spthead = ((JasperTerm) head).getSPTerm();
            SPTerm spttail = ((JasperTerm) tail).getSPTerm();
            Token request = sendMessage(new Class<?>[] { Term.class, Term.class }, sp, "consList",
                    new Object[] { spthead, spttail });
            SPTerm spt = (SPTerm) request.getResult();
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term newObjectTerm(Object obj) throws InterruptedException, ConversionFailedException, Exception {
            Token request = this.sendMessage(new Class<?>[] {}, sp, "newTerm", new Object[] {});
            SPTerm spt = (SPTerm) request.getResult();
            // [PD] 3.10.2 We can't call putObject directly since we are not
            // in the server thread.
            // spt.putObject(obj);
            this.sendMessage(new Class<?>[] { Object.class }, spt, "putObject", new Object[] { obj });
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term numberFromString(String str) throws InterruptedException, ConversionFailedException, Exception {
            Token request = this.sendMessage(new Class<?>[] {}, sp, "newTerm", new Object[] {});
            SPTerm spt = (SPTerm) request.getResult();
            spt.putNumberChars(str);
            return new JasperTerm(this, myServer, spt);
        }

        @Override
        public Term listFromString(String str) throws InterruptedException, ConversionFailedException, Exception {
            Token request = this.sendMessage(new Class<?>[] {}, sp, "newTerm", new Object[] {});
            SPTerm spt = (SPTerm) request.getResult();
            spt.putListChars(str);
            return new JasperTerm(this, myServer, spt);
        }

        void initProlog(PrologServer server) throws InterruptedException {
            this.sp = server.getSICStus();
            if (this.sp == null) {
                throw new Error("Can't get SICStus from server");
            }
            if (debugging(5))
                debugPrint("In JasperProlog.initProlog(); clientqueue==" + clientqueue);
        }

        JasperProlog(PrologServer server) throws InterruptedException {
            super(server);
            if (debugging(5))
                debugPrintln("Creating a JasperProlog");
        }

        JasperProlog(PrologServer server, SICStus sp) throws InterruptedException {
            super(server, sp);
            if (debugging(5))
                debugPrintln("Creating a JasperProlog");
        }
    }

    class JasperQuery implements Query {
        private final SPQuery spq;
        private final Map<String, Term> myMap;
        private final JasperProlog owner;

        @Override
        public void close() throws Exception {
            @SuppressWarnings("unused")
            Token dummy = owner.sendMessage(owner, new Class<?>[] {}, spq, "close", new Object[] {});
        }

        @Override
        public void cut() throws Exception {
            @SuppressWarnings("unused")
            Token dummy = owner.sendMessage(owner, new Class<?>[] {}, spq, "cut", new Object[] {});
        }

        @Override
        public boolean nextSolution() throws Exception {
            Token request = owner.sendMessage(owner, new Class<?>[] {}, spq, "nextSolution", new Object[] {});
            boolean result = ((Boolean) request.getResult()).booleanValue();
            if (result && myMap != null) {
                // Wrap all SPTerms in JasperTerm objects
                Iterator<Entry<String, Term>> it = myMap.entrySet().iterator();
                while (it.hasNext()) {
                    Entry<String, Term> me = it.next();
                    Object val = me.getValue();
                    if (val instanceof SPTerm) {
                        me.setValue(new JasperTerm(owner, jasperServer, (SPTerm) val));
                    }
                }
            }
            return result;
        }

        JasperQuery(JasperProlog owner, PrologServer server, String string, Map<String, Term> varMap) throws Exception {
            if (debugging(5))
                debugPrintln("Creating a JasperQuery");
            this.owner = owner;
            myMap = varMap;
            if (varMap != null) {
                // Unwrap all SPTerms
                Iterator<Entry<String, Term>> it = varMap.entrySet().iterator();
                while (it.hasNext()) {
                    Entry<String, Term> me = it.next();
                    Object val = me.getValue();
                    if (val instanceof JasperTerm) {
                        me.setValue(((JasperTerm) val).getSPTerm());
                    }
                }
            }
            Token request = owner.sendMessage(new Class<?>[] { String.class, Map.class }, owner.sp, "openQuery",
                    new Object[] { string, varMap });
            spq = (SPQuery) request.getResult();
        }
    }

    class JasperTerm implements Term {
        private final SPTerm spt;
        private final JasperProlog owner;

        SPTerm getSPTerm() {
            return spt;
        }

        // [PD] 3.9.1 Used by "glue code" (jasper.c). Must be called in the
        // server thread.
        long GetNativeTermRef() throws IllegalTermException {
            return getSPTerm().GetNativeTermRef();
        }

        @Override
        public int compare(Term with) throws IllegalTermException, Exception {
            SPTerm sptwith = ((JasperTerm) with).getSPTerm();
            Token request = owner.sendMessage(new Class<?>[] { Term.class }, spt, "compare", new Object[] { sptwith });
            return ((Integer) request.getResult()).intValue();
        }

        @Override
        public void delete() throws Exception {
            @SuppressWarnings("unused")
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "delete", new Object[] {});
        }

        @Override
        public Term getArg(int i, Term arg) throws InterruptedException, Exception {
            SPTerm sptarg = ((JasperTerm) arg).getSPTerm();
            @SuppressWarnings("unused")
            Token request = owner.sendMessage(new Class<?>[] { Integer.class, Term.class }, spt, "getArg",
                    new Object[] { Integer.valueOf(i), sptarg });
            return this;
        }

        @Override
        public double getDouble() throws ConversionFailedException, IllegalTermException, Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getDouble", new Object[] {});
            return ((Double) request.getResult()).doubleValue();
        }

        @Override
        public int getFunctorArity() throws ConversionFailedException, IllegalTermException, Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getFunctorArity", new Object[] {});
            return ((Integer) request.getResult()).intValue();
        }

        @Override
        public String getFunctorName() throws ConversionFailedException, IllegalTermException, Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getFunctorName", new Object[] {});
            return (String) request.getResult();
        }

        @Override
        public long getInteger() throws Exception {
            if (debugging(5))
                debugPrintln("entering JasperTerm.getInteger()");
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getInteger", new Object[] {});
            if (debugging(10))
                debugPrintln("in JasperTerm.getInteger(), returning");
            return ((Long) request.getResult()).longValue();
        }

        @Override
        public Term getList(Term head, Term tail) throws InterruptedException, Exception {
            SPTerm spthead = ((JasperTerm) head).getSPTerm();
            SPTerm spttail = ((JasperTerm) tail).getSPTerm();
            @SuppressWarnings("unused")
            Token request = owner.sendMessage(new Class<?>[] { Term.class, Term.class }, spt, "getList",
                    new Object[] { spthead, spttail });
            return this;
        }

        @Override
        public String getListChars() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getListChars", new Object[] {});
            return (String) request.getResult();
        }

        @Override
        public String getNumberChars() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getNumberChars", new Object[] {});
            return (String) request.getResult();
        }

        @Override
        public Object getObject() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getObject", new Object[] {});
            return request.getResult();
        }

        @Override
        public String getString() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "getString", new Object[] {});
            return (String) request.getResult();
        }

        @Override
        public boolean isAtom() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isAtom", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isAtomic() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isAtomic", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isCompound() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isCompound", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isEmptyList() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isEmptyList", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isFloat() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isFloat", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isInteger() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isInteger", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isList() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isList", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isNumber() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isNumber", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isValid() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isValid", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public boolean isVariable() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "isVariable", new Object[] {});
            return ((Boolean) request.getResult()).booleanValue();
        }

        @Override
        public String toString()
        // If we call SPTerm.toString() (via sendMessage()), this method must
        // throw Exception. The compiler will complain about that since
        // Object.toString() does not throw Exception.
        // Solution: catch the Exception from sendMessage() and return the
        // result from super.toString (which is Object.toString) with the
        // exception string appended.
        {
            if (debugging(5))
                debugPrintln("Entering JasperTerm.toString()");
            try {
                Token request = owner.sendMessage(new Class<?>[] {}, spt, "toString", new Object[] {});
                return (String) request.getResult();
            } catch (Exception ex) {
                if (debugging(2))
                    debugPrintln("Warning: SPTerm.toString() failed. Using Object.toString instead.");
                if (debugging(5)) {
                    debugPrintln("    exception:" + ex);
                    ex.printStackTrace(System.err);
                }
                return super.toString() + ex.toString();
            }
        }

        @Override
        public String toString(Term options) throws Exception {
            if (debugging(5))
                debugPrintln("Entering JasperTerm.toString(Term options)");
            if (debugging(5))
                debugPrintln("    spt.getClass()==" + spt.getClass());
            SPTerm sptoptions = ((JasperTerm) options).getSPTerm();
            Token request = owner.sendMessage(new Class<?>[] { Term.class }, spt, "toString",
                    new Object[] { sptoptions });
            if (debugging(5))
                debugPrintln("Returning from JasperTerm.toString(Term options)");
            return (String) request.getResult();
        }

        @Override
        public Term[] toPrologTermArray() throws InterruptedException, Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "toTermArray", new Object[] {});
            SPTerm[] spta = (SPTerm[]) request.getResult();
            Term[] ta = new Term[spta.length];
            for (int i = 0; i < ta.length; i++) {
                ta[i] = new JasperTerm(owner, jasperServer, spta[i]);
            }
            return ta;
        }

        @Override
        public int type() throws Exception {
            Token request = owner.sendMessage(new Class<?>[] {}, spt, "type", new Object[] {});
            return ((Integer) request.getResult()).intValue();
        }

        @Override
        public boolean unify(Term with) throws Exception {
            SPTerm sptwith = ((JasperTerm) with).getSPTerm();
            Token request = owner.sendMessage(new Class<?>[] { Term.class }, spt, "unify", new Object[] { sptwith });
            return ((Boolean) request.getResult()).booleanValue();
        }

        JasperTerm(JasperProlog owner, PrologServer server, SPTerm spt) throws InterruptedException {
            if (debugging(5))
                debugPrintln("Creating a JasperTerm");
            this.owner = owner;
            this.spt = spt;
        }
    }

    /* Meta-method */
    void callMethod(SICStus sp, Token request) {
        if (debugging(5))
            debugPrintln("Entering callMethod()");
        try {
            Object result = null;
            Object obj = request.getObject();
            String methodname = request.getMethodname();
            Class<?>[] ts = request.getTypesig();
            Object[] args = request.getArgs();
            if (methodname.equals("<init>")) {
                Class<?> clazz = (Class<?>) (obj);
                if (debugging(5)) {
                    debugPrintln("                     clazz==" + clazz);
                }
                Constructor<?> cons = clazz.getConstructor(ts);
                result = cons.newInstance(args);
            } else {
                Class<?> clazz = (request.getStaticP() ? (Class<?>) obj : obj.getClass());
                Method m = clazz.getMethod(methodname, request.getTypesig());
                result = m.invoke(obj, args);
            }
            if (result == null) {
                request.setResult(java.lang.Void.TYPE);
            } else {
                request.setResult(result);
            }
        } catch (Exception ex) {
            if (debugging(5))
                debugPrint("in callMethod(), caught an Exception ");
            if (debugging(5)) {
                if (ex instanceof InvocationTargetException) {
                    System.err.print(", InvocationTargetException with cause: ");
                    Throwable cause = ex.getCause();
                    System.err.println(cause.toString());
                    cause.printStackTrace(System.err);
                } else {
                    System.err.println(ex.toString());
                    ex.printStackTrace(System.err);
                }
            } else {
                if (debugging(5))
                    debugPrintln(""); // *** ???
            }
            request.setResult(java.lang.Void.TYPE);
            request.setException(ex);
        }
    }

    static ThreadLocal<Stack<Prolog>> JasperGlobalCallerStack = new ThreadLocal<Stack<Prolog>>() {
        @Override
        protected Stack<Prolog> initialValue() {
            Stack<Prolog> st = new Stack<>();
            st.push(null);
            return st;
        }
    };

    // *** [PD] FIX: This needs more detailed documentation.
    /**
     * Returns the Prolog interface corresponding to the SICStus runtime that called
     * us. If Java is the top level, this method returns null,
     * 
     * @return the calling instance
     */
    public static Prolog getCaller() {
        return (Prolog) ((Stack<?>) (JasperGlobalCallerStack.get())).peek();
    }

    // The arguments to newProlog must match the constructors for
    // class SICStus.
    /**
     * Creates a {@link Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, null, null)}.
     * 
     * @return a new prolog instance
     * @throws InterruptedException
     *             if the thread was interrupted
     */
    public static Prolog newProlog() throws InterruptedException {
        return newProlog(null, null, null);
    }

    /**
     * Creates a {@link Prolog} interface object. Equivalent to
     * {@link #newProlog(String[], String, String) newProlog(null, bootPath, null)}.
     * 
     * @param bootPath
     *            The path where SICStus should look for its start-up files.
     * @return a new prolog instance
     * @throws InterruptedException
     *             if the thread was interrupted
     */
    public static Prolog newProlog(String bootPath) throws InterruptedException {
        return newProlog(null, bootPath, null);
    }

    /**
     * Creates a {@link Prolog} interface object. Starts a server thread which will
     * serve that {@link Prolog}. The server thread takes care of all interaction
     * with the Prolog runtime, making sure that all calls to the Prolog runtime
     * will be done from one and the same thread.
     *
     * @param argv
     *            Argument vector to the emulator.
     * @param bootPath
     *            Ignored.
     * @param savFile
     *            A .sav-file to restore. See {@link SICStus#restore}
     * @return a new prolog instance
     * @throws InterruptedException
     *             if the thread was interrupted
     */
    public static Prolog newProlog(String[] argv, String bootPath, String savFile) throws InterruptedException {
        Jasper js = new Jasper(argv, savFile);
        return js.jasperProlog;
    }

    /**
     * Multiple threads must synchronize when calling this method.
     *
     *
     */
    /* [PD] 3.9 Should not have to be public. */
    // public static Prolog newProlog(SICStus sp) throws InterruptedException
    static Prolog newProlog(SICStus sp) throws InterruptedException {
        Jasper js = new Jasper(sp);
        return js.jasperProlog;
    }

    void startServer() {
        Thread serverThread = new Thread(jasperServer, "ServerThread");
        serverThread.setDaemon(true);
        serverThread.start();
    }

    Jasper(String[] argv, String savFile) throws InterruptedException {
        tokencounter = new Counter();
        queuecounter = new Counter();
        jasperServer = new PrologServer(argv, savFile);
        // Order is important here. jasperServer must exist when a new JasperProlog is
        // created.
        jasperProlog = new JasperProlog(jasperServer);
        // Order is important here, too. The server must be running before jasperProlog
        // is initialized.
        startServer();
        jasperProlog.initProlog(jasperServer);
    }

    Jasper(SICStus sp) throws InterruptedException {
        tokencounter = new Counter();
        queuecounter = new Counter();
        jasperServer = new PrologServer(sp);
        // Order is important here. jasperServer must exist when a new JasperProlog is
        // created.
        jasperProlog = new JasperProlog(jasperServer, sp);
    }

    /*-
     * Test Jasper.
     *  java -Djava.library.path=/usr/local/lib -Dsicstus.path=/usr/local/lib/sicstus-3.8.6 Jasper
     */
    /**
     * This is a small test function. It will try to load Jasper by creating a
     * Jasper object and prints a message if it succeeded.
     * 
     * @param argv
     *            the command line arguments
     */

    public static void main(String argv[]) {
        try {
            System.out.print("Trying to start Jasper...");
            @SuppressWarnings("unused")
            Prolog j = newProlog();
            System.out.println("OK");
        } catch (Exception ex) {
            System.err.println("Failed to start Jasper: ");
            ex.printStackTrace(System.err);
        }
    }

}
