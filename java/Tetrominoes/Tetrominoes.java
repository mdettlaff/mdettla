/*
  <applet width=385 height=482 code="Tetrominoes.class">
    <param name="backgroundColor" value="#FFFFFF">
    <param name="boardColor" value="#D0D0D0">
    <param name="audioFile" value="theme.au">
  </applet>
 */

import java.applet.Applet;
import java.applet.AudioClip;
import java.lang.Thread;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.util.LinkedList;
import java.util.Random;
import java.util.Date;

/**
 * Gra w stylu klasycznego Tetrisa.
 *
 * @author Micha³ Dettlaff
 * @version 1.6
 */
public class Tetrominoes extends Applet
    implements Runnable, KeyListener {

  static final int BOARD_WIDTH=10;
  static final int BOARD_HEIGHT=22;
  static final int BLOCK_SIZE=24;
  static final int MAX_LEVEL=20;
  static final int INIT_SPEED=800;
  Random rand;
  LinkedList<Tetromino> bag;
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
  boolean showTitle;
  boolean gameOver;
  boolean musicOn;
  AudioClip themeMusic;
  Color[][] board;
  Tetromino tetromino;
  Tetromino nextTetromino;

  public void init() {
    backgroundColor = Color.WHITE;
    boardColor = new Color(208, 208, 208);
    if (getParameter("backgroundColor") != null) {
      backgroundColor = Color.decode(getParameter("backgroundColor"));
    }
    if (getParameter("boardColor") != null) {
      boardColor = Color.decode(getParameter("boardColor"));
    }
    setBackground(backgroundColor);
    if (getParameter("audioFile") != null) {
      themeMusic = getAudioClip(getCodeBase(), getParameter("audioFile"));
    } else {
      themeMusic = getAudioClip(getCodeBase(), "theme.au");
    }
    musicOn=true;

    width=getWidth();
    height=getHeight();
    t = null;
    rand = new Random(new Date().getTime());
    bag = new LinkedList<Tetromino>();

    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();

    addKeyListener(this);
  }

  public void destroy() {
    removeKeyListener(this);
  }

  public void run() {
    showTitle();
    newGame();
    while (true) {
      paintBuffer();
      delay(speed);
      tetromino.dropSoft();
      if (isConflict(tetromino, board)) {
	tetromino.undoDropSoft();
	stickOn(tetromino, board);
	clearFullLines(board);
	tetromino = nextTetromino;
	nextTetromino = getTetromino(bag);
	if (isConflict(tetromino, board)) {
	  gameOver();
       	}
      }
    }
  }

  void showTitle() {
    board = new Color[BOARD_WIDTH][BOARD_HEIGHT];
    tetromino = new Tetromino();
    nextTetromino = new Tetromino();
    nextTetromino.hide();
    showTitle=true;
    paintBuffer();
    while (showTitle); // pauza az do wcisniecia entera lub spacji
  }

  void newGame() {
    board = new Color[BOARD_WIDTH][BOARD_HEIGHT];
    bag.clear();
    tetromino = getTetromino(bag);
    nextTetromino = getTetromino(bag);
    score=0;
    lines=0;
    level=0;
    speed=INIT_SPEED;
    if (musicOn) {
      themeMusic.play();
    }
  }

  void gameOver() {
    gameOver=true;
    paintBuffer();
    while (gameOver); // pauza az do wcisniecia entera lub spacji
    newGame();
  }

  /** Przykleja tetromino do planszy na stale. */
  void stickOn(Tetromino tetromino, Color[][] board) {
    int x, y;
    tetromino.immobilize();
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (tetromino.shape[i][j] == 1) {
	  x=tetromino.x+i;
	  y=tetromino.y+j;
	  board[x][y]=tetromino.color;
	}
      }
    }
  }

  void clearFullLines(Color[][] board) {
    boolean isLineFull;
    int linesCleared=0;

    tetromino.hide();
    for (int j=BOARD_HEIGHT-1; j >= 0; j--) { // zaczynamy od dolu planszy
      isLineFull=true;
      for (int i=0; i < BOARD_WIDTH; i++) {
	if (board[i][j] == null) {
	  isLineFull=false;
	}
      }
      if (isLineFull) {
	for (int k=0; k < BOARD_WIDTH; k++) { // podswietl przed skasowaniem
	  board[k][j] = brighter(board[k][j]);
	}
	paintBuffer();
	delay(150);
	for (int l=j; l > 0; l--) { // skasuj j-ta linie
	  for (int k=0; k < BOARD_WIDTH; k++) {
	    board[k][l] = board[k][l-1];
	  }
	}
	for (int k=0; k < BOARD_WIDTH; k++) {
	  board[k][0] = null;
	}
	paintBuffer();
	delay(150);
	linesCleared++;
	j++;
      }
    }
    if (linesCleared > 0) {
      switch (linesCleared) {
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
      level = lines / 10;
      if (level > MAX_LEVEL) { level=MAX_LEVEL; }
      speed = INIT_SPEED - level * 36;
    }
  }

  Color brighter(Color color) {
    int r=color.getRed();
    int g=color.getGreen();
    int b=color.getBlue();
    r+=128; g+=128; b+=128;
    if (r > 255) { r=255; }
    if (g > 255) { g=255; }
    if (b > 255) { b=255; }
    return new Color(r, g, b);
  }

  /**
   * Tetromina losowane sa z paczki, w ktorej jest 7 roznych tetromin w losowej
   * kolejnosci. Po oproznieniu paczki zapelniamy ja od nowa wedlug tej zasady.
   * Jest to tzw. Random Generator z Tetrisa.
   */
  Tetromino getTetromino(LinkedList<Tetromino> bag) {
    Tetromino t;

    if (bag.isEmpty()) { // zapelnij paczke od nowa
      while (bag.size() < 7) {
	t = getRandomTetromino(new TetrominoShapes(), rand);
	if (!contains(bag, t)) {
	  bag.addLast(t);
	}
      }
    }
    return bag.removeLast();
  }

  boolean contains(LinkedList<Tetromino> bag, Tetromino t) {
    for (int i=0; i < bag.size(); i++) {
      if (bag.get(i).color == t.color) {
	return true;
      }
    }
    return false;
  }

  Tetromino getRandomTetromino(TetrominoShapes tetrominoes, Random rand) {
    Tetromino tetromino = new Tetromino();
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
	tetromino.moveRight(); // zeby bylo na srodku planszy
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
	  x=tetromino.x+i;
	  y=tetromino.y+j;
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

  /** Czeka zadana ilosc milisekund. */
  void delay(int interval) {
    try {
      t.sleep(interval);
      // teraz watek sprawdza czy powinien sie wstrzymac
      if (threadSuspended) {
	synchronized(this) {
	  while (threadSuspended) {
	    wait();
	  }
	}
      }
    } catch (InterruptedException e) { }
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

  public void paint(Graphics g) {
    update(g);
  }

  void paintBuffer() {
    drawFrameInBuffer();
    repaint();
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync(); // ponoc polepsza wyglad w niektorych systemach
  }

  void drawFrameInBuffer() {
    buffer.clearRect(0, 0, width, height);
    buffer.setColor(boardColor);
    // widzialna czesc planszy jest o dwie linie nizsza od rzeczywistej
    buffer.fillRect(0, 0, BOARD_WIDTH*BLOCK_SIZE+1,
			      (BOARD_HEIGHT-2)*BLOCK_SIZE+1);
    // rysujemy plansze
    for (int j=2; j < BOARD_HEIGHT; j++) {
      for (int i=0; i < BOARD_WIDTH; i++) {
	if (board[i][j] != null) {
	  drawBlock(i, j, board[i][j]);
	}
      }
    }
    // rysujemy biezace tetromino
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (tetromino.shape[i][j] == 1) {
	  drawBlock(tetromino.x+i, tetromino.y+j, tetromino.color);
	}
      }
    }
    // rysujemy nastepne tetromino w kolejce
    for (int j=0; j < 4; j++) {
      for (int i=0; i < 4; i++) {
	if (nextTetromino.shape[i][j] == 1) {
	  drawBlock(BOARD_WIDTH+1+i, 4+j, nextTetromino.color);
	}
      }
    }
    buffer.setColor(Color.BLACK);
    buffer.drawRect(0, 0, BOARD_WIDTH*BLOCK_SIZE+1,
			      (BOARD_HEIGHT-2)*BLOCK_SIZE+1);

    buffer.setFont(new Font("Serif", Font.PLAIN, 17));
    buffer.drawString("Nastêpne:", BOARD_WIDTH*BLOCK_SIZE+28, 32);
    buffer.drawString("Punkty:", BOARD_WIDTH*BLOCK_SIZE+32, 145);
    buffer.drawString(score + "", BOARD_WIDTH*BLOCK_SIZE+32, 165);
    buffer.drawString("Linie:", BOARD_WIDTH*BLOCK_SIZE+32, 205);
    buffer.drawString(lines + "", BOARD_WIDTH*BLOCK_SIZE+32, 225);
    buffer.drawString("Poziom:", BOARD_WIDTH*BLOCK_SIZE+32, 265);
    buffer.drawString(level + "", BOARD_WIDTH*BLOCK_SIZE+32, 285);
    buffer.drawString("N - nowa gra", BOARD_WIDTH*BLOCK_SIZE+30, 345);
    buffer.drawString("P - pauza", BOARD_WIDTH*BLOCK_SIZE+30, 370);
    buffer.drawString("M - muzyka", BOARD_WIDTH*BLOCK_SIZE+30, 395);
    if (musicOn) {
      buffer.drawString("ON/off", BOARD_WIDTH*BLOCK_SIZE+55, 418);
    } else {
      buffer.drawString("on/OFF", BOARD_WIDTH*BLOCK_SIZE+55, 418);
    }

    if (gameOver) {
      buffer.setFont(new Font("Serif", Font.PLAIN, 24));
      buffer.drawString("Koniec gry", width/2-127, height/2-30);
    }
    if (showTitle) {
      int w=BOARD_WIDTH*BLOCK_SIZE/2;
      int h=BOARD_HEIGHT*BLOCK_SIZE/2;
      buffer.setFont(new Font("Serif", Font.PLAIN, 24));
      buffer.drawString("Tetrominoes", w-65, h-160);
      buffer.setFont(new Font("Serif", Font.PLAIN, 17));
      buffer.drawString("kod:", w-16, h-70);
      buffer.drawString("Micha³ Dettlaff", w-56, h-45);
      buffer.drawString("muzyka:", w-28, h-10);
      buffer.drawString("Micha³ Dettlaff", w-56, h+15);
    }
  }

  void drawBlock(int x, int y, Color color) {
    int bs=BLOCK_SIZE;
    buffer.setColor(color);
    // wszystko rysujemy o dwa bloki wyzej, bo pierwsze dwie linie sa ukryte
    buffer.fillRect(x*bs+1, (y-2)*bs+1, bs, bs);
    buffer.setColor(brighter(color));
    buffer.drawRect(x*bs+1, (y-2)*bs+1, bs-1, bs-1);
    buffer.drawRect(x*bs+2, (y-2)*bs+2, bs-3, bs-3);
    buffer.drawRect(x*bs+3, (y-2)*bs+3, bs-5, bs-5);
    buffer.setColor(color.darker());
    buffer.drawLine(x*bs+1, (y-1)*bs-0, (x+1)*bs-1, (y-1)*bs-0); // pozioma
    buffer.drawLine((x+1)*bs-0, (y-2)*bs+1, (x+1)*bs-0, (y-1)*bs-0); // pionowa
    buffer.drawLine(x*bs+2, (y-1)*bs-1, (x+1)*bs-2, (y-1)*bs-1); // pozioma
    buffer.drawLine((x+1)*bs-1, (y-2)*bs+2, (x+1)*bs-1, (y-1)*bs-1); // pionowa
    buffer.drawLine(x*bs+3, (y-1)*bs-2, (x+1)*bs-3, (y-1)*bs-2); // pozioma
    buffer.drawLine((x+1)*bs-2, (y-2)*bs+3, (x+1)*bs-2, (y-1)*bs-2); // pionowa
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
	  // obracanie mozliwe rowniez przy krawedziach planszy
	  if (isConflict(tetromino, board)) {
	    tetromino.moveRight();
	    if (isConflict(tetromino, board)) {
	      tetromino.moveLeft();
	      tetromino.moveLeft();
	      if (isConflict(tetromino, board)) {
	        tetromino.moveRight();
	      }
	    }
	    if (isConflict(tetromino, board)) {
	      tetromino.rotateCCW();
	    }
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
	  // obracanie mozliwe rowniez przy krawedziach planszy
	  if (isConflict(tetromino, board)) {
	    tetromino.moveRight();
	    if (isConflict(tetromino, board)) {
	      tetromino.moveLeft();
	      tetromino.moveLeft();
	      if (isConflict(tetromino, board)) {
	        tetromino.moveRight();
	      }
	    }
	    if (isConflict(tetromino, board)) {
	      tetromino.rotateCW();
	    }
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
      case ' ':
	showTitle=false;
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
       	// po ekranie tytulowym lub koncu gry nowa gra jest automatycznie
	if (!showTitle && !gameOver) {
	  newGame();
	  paintBuffer();
	}
	showTitle=false;
	gameOver=false;
	break;
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
      case 'M':
      case 'm':
	if (musicOn) {
	  themeMusic.stop();
	}
	musicOn=!musicOn;
	paintBuffer();
	break;
    }
    e.consume();
  }

  public String getAppletInfo() {
    return "Gra w stylu klasycznego Tetrisa.";
  }
}

class Tetromino {
  int size;
  short[][] shape;
  Color color;
  int x, y;
  boolean movable;

  Tetromino() {
    shape = new short[4][4];
    x=Tetrominoes.BOARD_WIDTH/2-2;
    y=1;
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
    y++;
  }

  void undoDropSoft() {
    y--;
  }

  void moveLeft() {
    x--;
  }

  void moveRight() {
    x++;
  }
}

/**
 * Przechowuje rodzaje tetromin. Zostaja zainicjalizowane dopiero po utworzeniu
 * instancji klasy TetrominoShapes.
 */
class TetrominoShapes {
  public final Tetromino I, J, L, O, S, T, Z;

  TetrominoShapes() {
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

