
/*
 * Note:This code is very old and does not reflect current best practices in Java.
 */

import java.awt.Canvas;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics;
import java.awt.Menu;
import java.awt.MenuBar;
import java.awt.MenuItem;
import java.awt.MenuShortcut;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.HashMap;

import se.sics.jasper.Jasper;
import se.sics.jasper.Prolog;
import se.sics.jasper.Query;
import se.sics.jasper.SICStus;
import se.sics.jasper.SPException;
import se.sics.jasper.Term;

class QueensCanvas extends Canvas {
    private static final long serialVersionUID = 1L;

    private final int boardSize;
    private final int cellSize;
    private final int SPACING = 0;
    private final int OFFSET = 10;
    private final boolean queens[][];

    public QueensCanvas(int boardSize, int cellSize) {
        int canvasSize = boardSize * cellSize + (boardSize - 1) * this.SPACING + this.OFFSET * 2;

        this.setSize(canvasSize, canvasSize + this.OFFSET * 2);

        this.boardSize = boardSize;
        this.cellSize = cellSize;

        queens = new boolean[boardSize][boardSize];
    }

    private Color getCellColor(int i, int j) {
        if ((i + j) % 2 == 0)
            return Color.black;
        else
            return Color.white;
    }

    @Override
    public void paint(Graphics g) {
        repaintBoard(g);
    }

    @Override
    public void update(Graphics g) {
        repaintBoard(g);
    }

    public void repaintBoard(Graphics g) {
        int i, j;

        for (i = 0; i < boardSize; i++) {
            for (j = 0; j < boardSize; j++) {
                if (queens[i][j]) {
                    drawQueen(g, i, j);
                } else {
                    g.setColor(getCellColor(i, j));
                    g.fillRect(OFFSET + i * (cellSize + SPACING), OFFSET + j * (cellSize + SPACING), cellSize,
                            cellSize);
                }
            }
        }

        /*
         * for (i = 0; i < boardSize; i++) { for (j = 0; j < boardSize; j++) {
         * drawAttacks(g,i,j); } }
         */
    }

    public void drawAttacks(Graphics g, int x, int y) {
        if (queens[x][y]) {
            g.setColor(Color.red);

            g.drawLine(OFFSET, OFFSET + y * cellSize + cellSize / 2, OFFSET + (boardSize * cellSize),
                    OFFSET + y * cellSize + cellSize / 2);

            g.drawLine(OFFSET + x * cellSize + cellSize / 2, OFFSET, OFFSET + x * cellSize + cellSize / 2,
                    OFFSET + (boardSize * cellSize));

        }
    }

    public void drawQueen(Graphics g, int i, int j) {
        g.setColor(getCellColor(i, j));
        g.fillRect(OFFSET + i * (cellSize + SPACING), OFFSET + j * (cellSize + SPACING), cellSize, cellSize);

        g.setColor(Color.blue);
        g.fillOval(OFFSET + i * (cellSize + SPACING) + 3, OFFSET + j * (cellSize + SPACING) + 3, cellSize - 6,
                cellSize - 6);
    }

    public void place(int x, int y, boolean hasQueen) {
        queens[x][y] = hasQueen;

        this.repaint();
    }

    public void removeAllQueens() {
        int i, j;

        for (i = 0; i < boardSize; i++)
            for (j = 0; j < boardSize; j++)
                queens[i][j] = false;
    }
}

class QueensFrame extends Frame {
    private static final long serialVersionUID = 1L;

    MenuBar menubar;
    Menu filemenu;
    MenuItem itemQuit, itemRestart, itemNextSol, itemSep;
    QueensCanvas board;

    Query jquery = null;
    Term size, res, pofile;
    Term res_array[];
    Prolog pp;
    Font font;

    public QueensFrame(int boardSize, int cellSize, Prolog p) throws SPException, Exception {
        pp = p;
        size = pp.newTerm(boardSize);
        res = pp.newVariable();

        setLayout(new FlowLayout()); // [PM] 3.11.1 needed for MacOS X

        font = new Font("Helvetica", Font.PLAIN, 12);

        menubar = new MenuBar();
        filemenu = new Menu("File");
        itemQuit = new MenuItem("Quit", new MenuShortcut(KeyEvent.VK_Q));
        itemRestart = new MenuItem("Restart", new MenuShortcut(KeyEvent.VK_R));
        itemNextSol = new MenuItem("Next Solution", new MenuShortcut(KeyEvent.VK_N));
        itemSep = new MenuItem("-");

        board = new QueensCanvas(boardSize, cellSize);
        this.add(board);

        itemQuit.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                System.exit(0);
            }
        });

        itemRestart.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    actionRestart();
                }
                // catch ( SPException spe ) {}
                catch (Exception spe) { // 3.9 **** Note: Exceptionhandling may
                                        // change to throwing more specific
                                        // exceptions in future versions of Jasper.
                    System.err.println("itemRestart failed to call actionRestart:");
                    spe.printStackTrace(System.err);
                }
            }
        });

        itemNextSol.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                try {
                    actionNextSol();
                }
                // catch ( SPException spe ) {}
                catch (Exception spe) { // 3.9 **** Note: Exceptionhandling may
                                        // change to throwing more specific
                                        // exceptions in future versions of Jasper.
                    System.err.println("itemNextSol failed to call actionNextSol:");
                    spe.printStackTrace(System.err);
                }
            }
        });

        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });

        this.setMenuBar(menubar);
        menubar.add(filemenu);
        filemenu.add(itemRestart);
        filemenu.add(itemNextSol);
        filemenu.add(itemSep);
        filemenu.add(itemQuit);

        this.setTitle("JQueens");

        this.pack();

        this.setResizable(false);

        this.setVisible(true);

        actionRestart();
    }

    public void placeQueen(int x, int y, boolean onoff) {
        board.place(x, y, onoff);
    }

    public void placeQueens(Term qargs[]) {
        int i, x, y;

        board.removeAllQueens();

        for (i = 0; i < res_array.length; i++) {
            x = i;
            y = Integer.valueOf(res_array[i].toString()).intValue() - 1;

            placeQueen(x, y, true);
        }
    }

    public void actionRestart() throws SPException, Exception {
        if (jquery != null) {
            jquery.close();
        }

        {
            HashMap<String, Term> argmap = new HashMap<>();
            argmap.put("Size", size);
            argmap.put("Res", res);
            jquery = pp.openPrologQuery("jqueens:jqueens(Size, Res).", argmap);
        }
        actionNextSol();
    }

    public void actionNextSol() throws SPException, Exception {
        if (jquery.nextSolution()) {
            res_array = res.toPrologTermArray();

            if (res_array == null) {
                System.out.println("res is not a list");
            } else {
                placeQueens(res_array);
            }
        } else {
            System.out.println("no more solutions");
        }
    }
}

public class Queens {
    /*
     * argv[0] = board size argv[1] = number of solutions in batch mode
     */
    public static void main(String argv[]) {
        int bsize = ((argv.length > 0) ? (Integer.decode(argv[0])).intValue() : 8);
        int nsol = ((argv.length > 1) ? (Integer.decode(argv[1])).intValue() : 0);
        play(bsize, nsol);
    }

    public static void play(int boardsize, int nsol) {
        try {
            new Queens().startGame(boardsize, nsol);
        } catch (Exception spe) {
            System.err.println("Failed to create a Queens frame:");
            spe.printStackTrace(System.err);
            System.exit(2);
        }
    }

    public void startGame(int boardSize, int nsol) throws SPException, Exception {
        SICStus sp = SICStus.getCaller();
        Prolog pp;
        if (sp == null) {
            // Java is top level, we need a Prolog client object.
            System.out.println("Queens.startGame(): creating a new Prolog");
            pp = Jasper.newProlog();

            HashMap<String, Term> map = new HashMap<>();
            Term Path;
            String path = System.getProperty("Queens.codepath");
            if (path == null) {
                Path = pp.prologReadFromString("library('jasper/examples/jqueens').", null);
            } else {
                Path = pp.newTerm(path);
            }

            map.put("Path", Path);
            pp.queryCutFail("load_files(Path).", map);
            new Solutions(pp, nsol, boardSize).run();
        } else {
            // SICStus is top level, get a client object from the SICStus object.
            System.out.println("Queens.startGame(): found an existing Prolog");
            pp = sp.newProlog();
            new Solutions(pp, nsol, boardSize).start();
            // Start a Prolog server in this thread.
            // Does not return until some other thread calls sp.stopServer().
            sp.startServer();
        }
    }

    class Solutions extends Thread {
        Prolog pp;
        QueensFrame qf;
        int nsol, boardSize;

        Solutions(Prolog p, int n, int bs) {
            pp = p;
            nsol = n;
            boardSize = bs;
        }

        @Override
        public void run() {
            try {
                qf = new QueensFrame(boardSize, 20, pp);
                if (nsol > 0) {
                    for (int i = 1; i <= nsol; i++) {
                        Thread.sleep(1000);
                        qf.actionNextSol();
                    }
                    System.exit(0);
                }
                // [PD] 3.10.1 catch a Throwable so that nothing gets thrown out of here
            } catch (Throwable thr) {
                System.err.println("Failed to create a Queens frame: (thr==" + thr + ")");
                thr.printStackTrace(System.err);
                System.exit(1);
            }
        }
    }

}

/*-
 * Try to keep the original indentation style
 * Local variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * end:
 **/
