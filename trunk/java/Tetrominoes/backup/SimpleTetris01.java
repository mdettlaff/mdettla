/**
 * @author Micha³ Dettlaff
 * @version 1.0
 */

/*
 * <applet width=390 height=480 code="SimpleTetris.class">
 * <param name="backgroundColor" value="#FFFFFF">
 * <param name="boardColor" value="#D8D8D8">
 * </applet>
 */

import java.applet.Applet;
import java.lang.Thread;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.util.Random;
import java.util.Date;

/**
 * Gra w stylu klasycznego Tetrisa.
 */
public class SimpleTetris extends Applet
    implements Runnable, KeyListener {

  static final int BOARD_WIDTH=10;
  static final int BOARD_HEIGHT=22;
  static final int BLOCK_SIZE=24;
  Thread t;
  boolean threadSuspended;
  Graphics buffer;
  Image bufferImage;
  Color backgroundColor;
  Color boardColor;
  int width, height;
  int speed;
  int score;
  int lines;
  int level;
  boolean gameOver;
  Color[][] board;
  Tetromino tetromino;

  public void init() {
    t = null;
    width=getWidth();
    height=getHeight();
    backgroundColor = Color.WHITE;
    boardColor = new Color(216, 216, 216); // jasnoszary
    if (getParameter("backgroundColor") != null)
      backgroundColor = Color.decode(getParameter("backgroundColor"));
    if (getParameter("boardColor") != null)
      boardColor = Color.decode(getParameter("boardColor"));
    setBackground(backgroundColor);
    speed=600;

    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();

    addKeyListener(this);
  }

  public void destroy() {
    removeKeyListener(this);
  }

  public void run() {
    newGame();
    while (true) {
      paintBuffer();
      delay(speed);
      tetromino.dropSoft();
      if (isConflict(tetromino, board)) {
	tetromino.undoDropSoft();
	stickOn(tetromino, board);
	clearFullLines(board);
	tetromino = getRandomTetromino(new Tetrominoes());
	if (isConflict(tetromino, board)) {
	  gameOver();
       	}
      }
    }
  }

  void gameOver() {
    gameOver=true;
    paintBuffer();
    while (gameOver); // pauza az do wcisniecia klawisza ENTER
    newGame();
  }

  void newGame() {
    board = new Color[BOARD_WIDTH][BOARD_HEIGHT];
    tetromino = getRandomTetromino(new Tetrominoes());
    score=0;
    lines=0;
    level=0;
  }

  /** Przykleja tetromino do planszy na stale. */
  void stickOn(Tetromino tetromino, Color[][] board) {
    int x, y;
    tetromino.immobilize();
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (tetromino.shape[i][j] == 1) {
	  x=tetromino.position.x+i;
	  y=tetromino.position.y+j;
	  board[x][y]=tetromino.color;
	}
      }
    }
  }

  void clearFullLines(Color[][] board) {
    boolean isLineFull;
    int linesCleared=0;

    tetromino.hide();
    for (int j=0; j < BOARD_HEIGHT; j++) {
      isLineFull=true;
      for (int i=0; i < BOARD_WIDTH; i++) {
	if (board[i][j] == null) {
	  isLineFull=false;
	}
      }
      if (isLineFull) { // skasuj linie j-ta
	for (int l=j; l > 0; l--) {
	  for (int k=0; k < BOARD_WIDTH; k++) {
	    board[k][l] = board[k][l-1];
	  }
	}
	for (int k=0; k < BOARD_WIDTH; k++) {
	  board[k][0] = null;
	}
	linesCleared++;
	if (linesCleared > 0) {
	  paintBuffer();
	  delay(200);
	}
      }
    }
    if (linesCleared > 0) {
      switch (linesCleared) { // przyznawanie punktow
	case 1:
	  score += 40*(level+1);
	  break;
	case 2:
	  score += 100*(level+1);
	  break;
	case 3:
	  score += 300*(level+1);
	  break;
	case 4:
	  score += 1200*(level+1);
	  break;
      }
      lines += linesCleared;
      if (level < 20) { // maksymaly level to dwudziesty
	if (lines % 10 == 0) { // co dziesiec oczyszczonych linii level w gore
	  level++;
	  speed -= 20;
	}
      }
    }
  }

  Tetromino getRandomTetromino(Tetrominoes tetrominoes) {
    Tetromino tetromino = new Tetromino();
    Random rand = new Random((new Date()).getTime());
    switch (rand.nextInt(7)) { // losujemy rodzaj tetromina
      case 0:
	tetromino = tetrominoes.I;
	tetromino.setColor(Color.CYAN);
	break;
      case 1:
	tetromino = tetrominoes.J;
	tetromino.setColor(Color.BLUE);
	break;
      case 2:
	tetromino = tetrominoes.L;
	tetromino.setColor(Color.ORANGE);
	break;
      case 3:
	tetromino = tetrominoes.O;
	tetromino.setColor(Color.YELLOW);
	tetromino.moveRight(); // zeby bylo wyposrodkowane
	break;
      case 4:
	tetromino = tetrominoes.S;
	tetromino.setColor(Color.GREEN);
	break;
      case 5:
	tetromino = tetrominoes.T;
	tetromino.setColor(Color.MAGENTA);
	break;
      case 6:
	tetromino = tetrominoes.Z;
	tetromino.setColor(Color.RED);
	break;
    }
    tetromino.rotateCW(); // po inicjalizacji przekrecone, trzeba odkrecic
    return tetromino;
  }

  boolean isConflict(Tetromino tetromino, Color[][] board) {
    int x, y;
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (tetromino.shape[i][j] == 1) {
	  x=tetromino.position.x+i;
	  y=tetromino.position.y+j;
	  if (x < 0 || x > BOARD_WIDTH-1 || y < 0 || y > BOARD_HEIGHT-1) {
	    return true;
	  } else if (board[x][y] != null) { // null oznacza puste pole
	    return true;
	  }
	}
      }
    }
    return false;
  }

  public void start() {
    if (t == null) {
      t = new Thread(this);
      threadSuspended = false;
      t.start();
    }
    else {
      if (threadSuspended) {
	threadSuspended = false;
	synchronized(this) {
	  notify();
	}
      }
    }
  }

  public void stop() {
    threadSuspended=true;
  }

  /** Czeka zadana ilosc milisekund. */
  void delay(int interval) {
    try {
      t.sleep(interval); // interval given in milliseconds
      // now the thread checks to see if it should suspend itself
      if (threadSuspended) {
	synchronized(this) {
	  while (threadSuspended)
	    wait();
	}
      }
    }
    catch (InterruptedException e) { }
  }

  public void paint(Graphics g) {
    update(g);
  }

  void paintBuffer() {
    drawInBuffer();
    repaint();
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync(); // ponoc polepsza wyglad w niektorych systemach
  }

  void drawInBuffer() {
    buffer.clearRect(0, 0, width, height);
    buffer.setColor(boardColor);
    // widzialna czesc planszy jest o dwie linie nizsza od rzeczywistej
    buffer.fillRect(0, 0, BOARD_WIDTH*BLOCK_SIZE, (BOARD_HEIGHT-2)*BLOCK_SIZE);

    // rysujemy biezace tetromino
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (tetromino.shape[i][j] == 1) {
	  drawBlock(tetromino.position.x+i, tetromino.position.y+j,
			 tetromino.color);
	}
      }
    }
    // rysujemy plansze
    for (int j=2; j < BOARD_HEIGHT; j++) {
      for (int i=0; i < BOARD_WIDTH; i++) {
	if (board[i][j] != null) {
	  drawBlock(i, j, board[i][j]);
	}
      }
    }
    buffer.setColor(Color.BLACK);
    buffer.drawRect(0, 0, BOARD_WIDTH*BLOCK_SIZE-1,
			    (BOARD_HEIGHT-2)*BLOCK_SIZE-1);

    buffer.setFont(new Font("Serif", Font.PLAIN, 17));
    buffer.drawString("Punkty:", 280, 120);
    buffer.drawString(score + "", 280, 140);
    buffer.drawString("Linie:", 280, 180);
    buffer.drawString(lines + "", 280, 200);
    buffer.drawString("Poziom:", 280, 240);
    buffer.drawString(level + "", 280, 260);

    if (gameOver) {
      buffer.setFont(new Font("Serif", Font.PLAIN, 20));
      buffer.drawString("Koniec gry", width/2-120, height/2-30);
    }
  }

  void drawBlock(int x, int y, Color color) {
    buffer.setColor(color);
    // wszystko rysujemy o dwa bloki wyzej, bo pierwsze dwie linie sa ukryte
    buffer.fillRect(x*BLOCK_SIZE, (y-2)*BLOCK_SIZE, BLOCK_SIZE, BLOCK_SIZE);
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    if (!threadSuspended && !gameOver && tetromino.isMovable()) {
      switch (e.getKeyCode()) {
	case KeyEvent.VK_LEFT:
	  tetromino.moveLeft();
	  if (isConflict(tetromino, board)) {
	    tetromino.moveRight();
	  }
	  break;
	case KeyEvent.VK_RIGHT:
	  tetromino.moveRight();
	  if (isConflict(tetromino, board)) {
	    tetromino.moveLeft();
	  }
	  break;
	case KeyEvent.VK_UP:
	  tetromino.rotateCW();
	  if (isConflict(tetromino, board)) {
	    tetromino.rotateCCW();
	  }
	  break;
	case KeyEvent.VK_DOWN:
	  tetromino.dropSoft();
	  if (isConflict(tetromino, board)) {
	    tetromino.undoDropSoft();
	    tetromino.immobilize();
	  }
	  break;
	case KeyEvent.VK_CONTROL:
	  tetromino.rotateCCW();
	  if (isConflict(tetromino, board)) {
	    tetromino.rotateCW();
	  }
	  break;
      }
      paintBuffer();
    }
    e.consume();
  }

  public void keyTyped(KeyEvent e) {
    switch (e.getKeyChar()) {
      case '\n':
	gameOver=false;
	break;
      case 'N':
      case 'n':
	if (threadSuspended) {
	  threadSuspended = false;
	  synchronized(this) {
	    notify();
	  }
	}
	gameOver=false;
	newGame();
	break;
      case ' ':
      case 'p':
      case 'P':
	if (threadSuspended) {
	  threadSuspended = false;
	  synchronized(this) {
	    notify();
	  }
	}
	else
	  threadSuspended = true;
	break;
    }
  }

  public String getAppletInfo() {
    return "Gra w stylu klasycznego Tetrisa.";
  }
}

class Tetromino {
  int size;
  short[][] shape;
  Color color;
  Coords position;
  boolean movable;

  Tetromino() {
    shape = new short[4][4];
    position = new Coords(SimpleTetris.BOARD_WIDTH/2-2, 0);
    movable=true;
  }

  Tetromino(int initSize, short[][] initShape) {
    this();
    size=initSize;
    shape=initShape;
  }

  void rotateCW() {
    short[][] shapeTmp = new short[4][4];
    for (int j=0; j < size; j++) {
      for (int i=0; i < size; i++) {
	shapeTmp[size-1-j][i]=shape[i][j];
      }
    }
    shape=shapeTmp;
  }

  void rotateCCW() {
    short[][] shapeTmp = new short[4][4];
    for (int j=0; j < size; j++) {
      for (int i=0; i < size; i++) {
	shapeTmp[j][size-1-i]=shape[i][j];
      }
    }
    shape=shapeTmp;
  }

  boolean isMovable() {
    return movable;
  }

  void immobilize() {
    movable=false;
  }

  void hide() {
    shape = new short[4][4];
    size=0;
  }

  void setColor(Color newColor) {
    color=newColor;
  }

  void dropSoft() {
    position.y++;
  }

  void undoDropSoft() {
    position.y--;
  }

  void moveLeft() {
    position.x--;
  }

  void moveRight() {
    position.x++;
  }
}

/**
 * Przechowuje rodzaje tetromin. Zostaja zainicjalizowane dopiero po utworzeniu
 * instancji klasy Tetrominoes.
 */
class Tetrominoes {
  public final Tetromino I, J, L, O, S, T, Z;

  Tetrominoes() {
    short[][] shapeI = {
      { 0,0,0,0 },
      { 1,1,1,1 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    I = new Tetromino(4, shapeI);
    short[][] shapeJ = {
      { 1,0,0,0 },
      { 1,1,1,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    J = new Tetromino(3, shapeJ);
    short[][] shapeL = {
      { 0,0,1,0 },
      { 1,1,1,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    L = new Tetromino(3, shapeL);
    short[][] shapeO = {
      { 1,1,0,0 },
      { 1,1,0,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    O = new Tetromino(2, shapeO);
    short[][] shapeS = {
      { 0,1,1,0 },
      { 1,1,0,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    S = new Tetromino(3, shapeS);
    short[][] shapeT = {
      { 0,1,0,0 },
      { 1,1,1,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    T = new Tetromino(3, shapeT);
    short[][] shapeZ = {
      { 1,1,0,0 },
      { 0,1,1,0 },
      { 0,0,0,0 },
      { 0,0,0,0 },
    };
    Z = new Tetromino(3, shapeZ);
  }
}

/**
 * Przechowuje wspolrzedne x oraz y.
 */
class Coords {
  int x, y;

  Coords(int initX, int initY) {
    x=initX;
    y=initY;
  }

  boolean equals(Coords coords) {
    return (coords.x == x && coords.y == y);
  }

  static Coords add(Coords c1, Coords c2) {
    return new Coords(c1.x+c2.x, c1.y+c2.y);
  }
}

