
/**
 * A simple example showing how to connect a Java GUI to a running
 * prolog server.
 *
 * Note:This code is very old and does not reflect current best practices in Java.
 */
import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

import se.sics.prologbeans.Bindings;
import se.sics.prologbeans.PBTerm;
import se.sics.prologbeans.PrologSession;
import se.sics.prologbeans.QueryAnswer;

class SyncObject {
    // Turns zero when shut down (i.e. when run() returns).
    private final CountDownLatch iIsShutDown = new CountDownLatch(1);
    // Turns zero when iPort has been set.
    private final CountDownLatch iPortAwailable = new CountDownLatch(1);
    // protected by iPortAwailable
    private int iPort = -1;

    /**
     *
     * Should be called (at most) once, to set the port.
     *
     * @param port
     */
    public void setPort(int port) {
        iPort = port;
        iPortAwailable.countDown();
    }

    /**
     * Get the port from the running {@cod PBtest} server (if not received within
     * the timeout, -1 will be returned.
     *
     * @param msTimeout
     * @return the port, or -1 on timeout.
     * @throws InterruptedException
     */
    public int getPort(int msTimeout) throws InterruptedException {
        if (iPortAwailable.await(msTimeout, TimeUnit.MILLISECONDS)) {
            return iPort;
        } else {
            return -1;
        }
    }

    /**
     *
     * Should be called once, when shut down.
     */
    public void setShutDown() {
        iIsShutDown.countDown();
    }

    /**
     * Wait until the server has been shut down, and returns {@code true}. If not
     * shut down within the timeout, -1 will be returned.
     *
     * @param msTimeout
     * @return whether shut down has happened
     * @throws InterruptedException
     */
    public boolean waitForShutdown(int msTimeout) throws InterruptedException {
        return iIsShutDown.await(msTimeout, TimeUnit.MILLISECONDS);
    }
}

public class EvaluateGUI implements ActionListener {
    private static final int msTimeout = Integer.getInteger("se.sics.prologbeans.timeout", 10 * 1000).intValue();

    private final JTextArea text = new JTextArea(20, 40);
    private final JTextField input = new JTextField(36);
    private final JButton iEvaluateButton = new JButton("Evaluate");
    // Valid once SyncObject.getPort() returns a positive port number
    private PrologSession iSession;

    public EvaluateGUI() throws java.io.IOException {
        this(null);
    }

    public EvaluateGUI(SyncObject syncObj) throws java.io.IOException {
        boolean isBatch = (syncObj != null);

        JFrame frame = new JFrame("Prolog Evaluator");
        Container panel = frame.getContentPane();
        panel.add(new JScrollPane(text), BorderLayout.CENTER);
        JPanel inputPanel = new JPanel(new BorderLayout());
        inputPanel.add(input, BorderLayout.CENTER);
        inputPanel.add(iEvaluateButton, BorderLayout.EAST);
        panel.add(inputPanel, BorderLayout.SOUTH);
        text.setEditable(false);
        iEvaluateButton.addActionListener(this);
        input.addActionListener(this);

        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.pack();
        try {
            if (isBatch) {

                int port = syncObj.getPort(msTimeout);

                if (port > 0) {
                    iSession = new PrologSession();
                    iSession.setPort(port);
                    if ((Integer.getInteger("se.sics.prologbeans.debugLevel", 0)).intValue() != 0) {
                        iSession.setTimeout(0);
                    } else {
                        iSession.setTimeout(msTimeout);
                    }
                    iSession.connect();

                    runBatchQueries();
                }

            } else {
                iSession = new PrologSession();
                iSession.connect();
            }
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
        frame.setVisible(true);
    }

    @Override
    public void actionPerformed(ActionEvent event) {
        String message;
        try {
            Bindings bindings = new Bindings().bind("E", input.getText() + '.');
            QueryAnswer answer = iSession.executeQuery("evaluate(E,R)", bindings);
            PBTerm result = answer.getValue("R");
            if (result != null) {
                message = input.getText() + " = " + result + '\n';
                text.append(message);
                input.setText("");
            } else {
                message = "Error: " + answer.getError() + '\n';
                text.append(message);
            }
        } catch (Exception e) {
            message = "Error when querying Prolog Server: " + e.getMessage() + '\n';
            text.append(message);
            e.printStackTrace();
        }
        // [PM] 4.2.1 debug
        System.err.println("Log: " + message);
    }

    private void runBatchQueries() {
        final Runnable doCLickOnEvaluateButton = new Runnable() {
            @Override
            public void run() {
                iEvaluateButton.doClick();
            }
        };

        new Thread() {
            @Override
            public void run() {
                try {
                    for (String q : Arrays.asList("4+6", "11-5", "3*7", "9/3")) {
                        input.setText(q);
                        SwingUtilities.invokeAndWait(doCLickOnEvaluateButton);
                    }
                    iSession.executeQuery("shutdown");
                    iSession.disconnect();
                    System.exit(0);
                } catch (Exception ex) {
                    text.append("Error when clicking Evaluate button: " + ex.getMessage() + '\n');
                    ex.printStackTrace();
                }
            }
        }.start();
    }

    public static void main(String[] args) throws java.io.IOException {
        boolean isBatch = args.length > 0 && args[0].equalsIgnoreCase("batch");

        if (isBatch) {
            final SyncObject syncObject = new SyncObject();
            Thread t = new Thread() {
                @Override
                public void run() {
                    try {
                        String prolog = System.getProperty("se.sics.prologbeans.prolog", "sicstus");
                        // The prolog file is assumed to be located in the working directory
                        String[] command = { prolog, "-l", "evaluate.pl", "--goal", "main(batch)." };
                        System.err.println("DBG: Launching SICStus with " + Arrays.toString(command));
                        System.err.flush();
                        Process process = Runtime.getRuntime().exec(command);

                        // Write all the error output that has no % in the start of the line
                        BufferedReader err = new BufferedReader(new InputStreamReader(process.getErrorStream()));
                        String line;
                        while ((line = err.readLine()) != null) {
                            if (line.length() > 0 && line.charAt(0) != '%') {
                                System.err.println(line);
                                System.err.flush();

                                // When port is found, set it and notify waiters that SICStus is running
                                String prefix = "port:";
                                if (line.startsWith(prefix)) {
                                    int port = Integer.parseInt(line.substring(prefix.length())); // e.g, port:4711
                                    syncObject.setPort(port);
                                }
                            }
                        }

                    } catch (Exception e) {
                        e.printStackTrace();
                        syncObject.setPort(-2);
                    } finally {
                        syncObject.setShutDown();
                    }
                }
            };
            t.setDaemon(true);
            t.start();

            ignore(new EvaluateGUI(syncObject));
        } else {
            ignore(new EvaluateGUI());
        }
    }

    // Ignores its argument. Used for suppressing warnings about unused objects.
    private static void ignore(Object ignore) {
        // empty
    }
}
