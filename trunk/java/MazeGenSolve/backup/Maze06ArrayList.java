/**
   @version 1.6
   @author Michal Dettlaff
 */

/* <applet width=490 height=385 code="Maze.class"></applet> */

import java.applet.*;
import java.awt.*;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Random;
import java.util.Date;
 
/**
 * Generuje labirynt przy pomocy algorytmu Depth-First Search,
 * a nastepnie rozwiazuje go przy pomocy algorytmu Tremaux
 */
public class Maze extends Applet implements Runnable {
 
  /**
   * klasa wewnetrzna reprezentujaca komorke labiryntu
   */
  class MazeCell {
    class Directions {
      boolean north;
      boolean south;
      boolean east;
      boolean west;
      Directions(boolean initNorth, boolean initSouth,
             boolean initEast, boolean initWest) {
        north=initNorth;
        south=initSouth;
        east=initEast;
        west=initWest;
      }
    }
    Directions backtrack;
    Directions solution;
    Directions border;
    Directions walls;
    MazeCell () { // konstruktor
      backtrack = new Directions(false, false, false, false);
      solution = new Directions(false, false, false, false);
      border = new Directions(false, false, false, false);
      // na poczatku wszystkie sciany stoja
      walls = new Directions(true, true, true, true);
    }

    /* metody sprawdzajace zawartosc komorki */
    // sprawdza, czy wszystkie sciany komorki stoja
    boolean isIntact() {
      if (walls.north && walls.south && walls.east && walls.west)
        return true;
      else
        return false;
    }
    // sprawdza, czy przez komorke przechodzi solution
    boolean isSolutionIn() {
      if (solution.north || solution.south || solution.east || solution.west)
        return true;
      else
        return false;
    }
    // sprawdza, czy przez komorke przechodzi backtrack
    boolean isBacktrackIn() {
      if (backtrack.north || backtrack.south
	  || backtrack.east || backtrack.west)
        return true;
      else
        return false;
    }
    // sprawdza, czy jest sciana z danej strony komorki
    boolean isWallInDirection(Coords direction) {
      if (direction.y < 0 && walls.north == true)
	return true; // jest sciana na polnoc
      if (direction.y > 0 && walls.south == true)
	return true; // jest sciana na poludnie
      if (direction.x > 0 && walls.east == true)
	return true; // jest sciana na wschod
      if (direction.x < 0 && walls.west == true)
	return true; // jest sciana na zachod
      return false;
    }
    // sprawdza, czy jest brzeg z danej strony komorki
    boolean isBorderInDirection(Coords direction) {
      if (direction.y < 0 && border.north == true)
	return true; // jest brzeg na polnoc
      if (direction.y > 0 && border.south == true)
	return true; // jest brzeg na poludnie
      if (direction.x > 0 && border.east == true)
	return true; // jest brzeg na wschod
      if (direction.x < 0 && border.west == true)
	return true; // jest brzeg na zachod
      return false;
    }

    /* metody modyfikujace zawartosc komorki */
    // zaznacza solution w kierunku z currentCell do newCell
    void markSolution(Coords currentCell, Coords newCell) {
      if (newCell.y < currentCell.y) // sasiad na polnosc
        solution.north=true;
      else if (newCell.y > currentCell.y) // poludnie
        solution.south=true;
      else if (newCell.x > currentCell.x) // wschod
        solution.east=true;
      else if (newCell.x < currentCell.x) // zachod
        solution.west=true;
    }
    // odznacza solution w kierunku z currentCell do newCell
    void unmarkSolution(Coords currentCell, Coords newCell) {
      if (newCell.y < currentCell.y) // sasiad na polnosc
        solution.north=false;
      else if (newCell.y > currentCell.y) // poludnie
        solution.south=false;
      else if (newCell.x > currentCell.x) // wschod
        solution.east=false;
      else if (newCell.x < currentCell.x) // zachod
        solution.west=false;
    }
    // zaznacza backtrack w kierunku z currentCell do newCell
    // przy okazji odznacza solution, zeby sie nie pokrywaly
    void markBacktrack(Coords currentCell, Coords newCell) {
      solution.north=solution.south=solution.east=solution.west=false;
      if (newCell.y < currentCell.y) // sasiad na polnosc
        backtrack.north=true;
      else if (newCell.y > currentCell.y) // poludnie
        backtrack.south=true;
      else if (newCell.x > currentCell.x) // wschod
        backtrack.east=true;
      else if (newCell.x < currentCell.x) // zachod
        backtrack.west=true;
    }
  }

  Thread t = null;
  boolean threadSuspended;
  Graphics buffer; // grafike bedziemy rysowac w buforze, zeby nie migalo
  Image bufferImage; // pojedyncza klatka, do ktorej wrzucamy gotowy bufor
  int drawingSpeed=25; // interval given in milliseconds
  int width, height; // rozmiary appletu
  int i;
  final int CELL_SIZE=15; // wielkosc komorki labiryntu w pikselach
  final int X_MAR=5; // margines boczny
  final int Y_MAR=5; // margines gorny i dolny
  int mazeWidth=32; // szerokosc labiryntu w komorkach
  int mazeHeight=25; // wysokosc labiryntu w komorkach
  // tablica reprezentujaca labirynt w postaci komorek
  MazeCell[][] maze = new MazeCell[mazeWidth][mazeHeight];
  Coords entry, exit; // wspolrzedne wejscia i wyjscia
  // pozycje sasiadow komorki (polnoc, poludnie, wschod, zachod)
  final Coords[] DIRECTIONS = {new Coords(0,-1), new Coords(0,1),
			       new Coords(1,0), new Coords(-1,0)};

  // metoda wykonuje sie przy uruchomieniu appleta
  public void init() {
    // szerokosc i wysokosc appletu
    width = getSize().width;
    height = getSize().height;
    setBackground(new Color(64, 64, 64)); // ciemnoszary

    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();
  }

  // Executed after the applet is created; and also whenever
  // the browser returns to the page containing the applet.
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

  // Executed whenever the browser leaves the page containing the applet.
  public void stop() {
    threadSuspended=true;
  }

  // Executed within the thread that this applet created.
  public void run() {
    try {
      while (true) {
	// Inicjalizujemy labirynt
	// wstawiamy wszedzie sciany
	for (int i=0; i < mazeWidth; i++)
	  for (int j=0; j < mazeHeight; j++)
	    maze[i][j] = new MazeCell();
	// nastepnie dodajemy brzegi
	for (int i=0; i < mazeWidth; i++) { // gorny i dolny brzeg labiryntu
	  maze[i][0].border.north=true;
	  maze[i][mazeHeight-1].border.south=true;
	}
	for (int j=0; j < mazeHeight; j++) { // lewy i prawy brzeg labiryntu
	  maze[0][j].border.west=true;
	  maze[mazeWidth-1][j].border.east=true;
	}
	// ukrywamy wejscie i wyjscie
	entry = exit = null;

	drawInBuffer(); // rysujemy zainicjalizowany labirynt
	repaint();
	t.sleep(500); // interval given in milliseconds

	/* Budujemy labirynt przez sukcesywne burzenie scian, stosujac
	 * Algorytm Depth-First Search generowania labiryntu
	 * algorytm tworzy labirynt typu perfect maze, tzn. taki, w ktorym
	 * istnieje dokladnie 1 sciezka miedzy kazdymi dwoma pozycjami */

	CoordsStack<Coords> cellStack = new CoordsStack<Coords>();
	int totalCells = mazeWidth * mazeHeight;
	Date date = new Date(); // czas uniksowy uzyjemy jako seed w rand
	Random rand = new Random(date.getTime()); // generator liczb losowych
	Coords currentCell = new Coords(rand.nextInt(mazeWidth),
					rand.nextInt(mazeHeight));
	int visitedCells = 1;

	while (visitedCells < totalCells) {
	  // find all neighbors of currentCell with all walls intact
	  // ArrayList to tablica o zmiennej dlugosci z interfejsem listy
	  ArrayList<Coords> intactNeighbors = new ArrayList<Coords>();
	  for (int i=0; i < DIRECTIONS.length; i++) { // kolejne kierunki
	    int x=DIRECTIONS[i].x+currentCell.x;
	    int y=DIRECTIONS[i].y+currentCell.y;
	    if (!maze[currentCell.x][currentCell.y].isBorderInDirection(DIRECTIONS[i]))
	      if (maze[x][y].isIntact())
		intactNeighbors.add(new Coords(x, y));
	  }
	  if (intactNeighbors.size() > 0) { // if one or more found 
	    int randInd=rand.nextInt(intactNeighbors.size()); // random index
	    // choose one at random
	    Coords newCell = new Coords(intactNeighbors.get(randInd).x,
					intactNeighbors.get(randInd).y);
	    knockDownWall(newCell, currentCell);

	    // obsluga animacji ilustrujacej dzialanie algorytmu
	    // Now the thread checks to see if it should suspend itself
	    if (threadSuspended) {
	      synchronized(this) {
		while (threadSuspended) {
		  wait();
		}
	      }
	    }
	    t.sleep(drawingSpeed); // interval given in milliseconds
	    drawInBuffer();
	    repaint();

	    cellStack.push(currentCell);
	    currentCell=newCell;
	    visitedCells++;
	  }
	  else 
	    currentCell=cellStack.pop();
	}

	t.sleep(1000);

	// umieszczamy wejscie i wyjscie
	switch (rand.nextInt(4)) {
	  case 0 : 
	    entry = new Coords(0, mazeHeight-1); // wspolrzedne wejscia
	    exit = new Coords(mazeWidth-1, 0); // wspolrzedne wyjscia
	    break;
	  case 1 : 
	    entry = new Coords(mazeWidth-1, 0);
	    exit = new Coords(0, mazeHeight-1);
	    break;
	  case 2 : 
	    entry = new Coords(0, 0);
	    exit = new Coords(mazeWidth-1, mazeHeight-1);
	    break;
	  case 3 : 
	    entry = new Coords(mazeWidth-1, mazeHeight-1);
	    exit = new Coords(0, 0);
	}

	drawInBuffer();
	repaint();
	t.sleep(1000);

	/* Algorytm Tremaux znajdujacy wyjscie z labiryntu
	 * Znajdzie rozwiazanie dla dowolnego labiryntu, ale niekoniecznie
	 * najkrotsze. Po zakonczeniu algorytmu albo znajdziemy wyjscie,
	 * albo przejdziemy caly labirynt i wrocimy na poczatek. */

	currentCell = new Coords(entry.x, entry.y);
	boolean endTremaux=false;
	while (!endTremaux) {
	  // znajdujemy dostepnych sasiadow bez bactrack i solution
	  // oraz dostepnych sasiadow bez backtrack, ale z solution
	  ArrayList<Coords> freeNeighbors = new ArrayList<Coords>();
	  ArrayList<Coords> neighborsWithSolution = new ArrayList<Coords>();
	  for (int i=0; i < DIRECTIONS.length; i++) {
	    int x=DIRECTIONS[i].x+currentCell.x;
	    int y=DIRECTIONS[i].y+currentCell.y;
	    if (!maze[currentCell.x][currentCell.y].isBorderInDirection(DIRECTIONS[i])) {
	      if (!maze[currentCell.x][currentCell.y].isWallInDirection(DIRECTIONS[i])
		&& !maze[x][y].isBacktrackIn() && !maze[x][y].isSolutionIn())
		freeNeighbors.add(new Coords(x, y));
	      else if (!maze[currentCell.x][currentCell.y].isWallInDirection(DIRECTIONS[i])
		&& !maze[x][y].isBacktrackIn())
		neighborsWithSolution.add(new Coords(x, y));
	    }
	  }
	  // jesli jestesmy u celu, koniec
	  if (currentCell.x == exit.x && currentCell.y == exit.y)
	    endTremaux=true;
	  else if (freeNeighbors.size() > 0) { // jesli obok sa wolne komorki
	    // wybieramy jedna losowo i idziemy w jej kierunku znaczac solution
	    int randInd=rand.nextInt(freeNeighbors.size()); // random index
	    Coords newCell = new Coords(freeNeighbors.get(randInd).x,
					freeNeighbors.get(randInd).y);
	    maze[currentCell.x][currentCell.y].markSolution(
					    currentCell, newCell);
	    currentCell = newCell;
	  }
	  // jesli jest obok komorka z solution do ktorej mozna pojsc
	  else if (neighborsWithSolution.size() > 0) {
	    // idziemy tam znaczac po drodze backtrack
	    // i odznaczajac solution nadchodzace z naprzeciwka
	    int randInd=rand.nextInt(neighborsWithSolution.size());
	    Coords newCell = new Coords(neighborsWithSolution.get(randInd).x,
					neighborsWithSolution.get(randInd).y);
	    maze[currentCell.x][currentCell.y].markBacktrack(
						currentCell, newCell);
	    maze[newCell.x][newCell.y].unmarkSolution(
						newCell, currentCell);
	    currentCell = newCell;
	  }
	  else // nie mozna juz isc dalej; wrocilismy na poczatek labiryntu
	    endTremaux=true;

	  // obsluga animacji ilustrujacej dzialanie algorytmu
	  // Now the thread checks to see if it should suspend itself
	  if (threadSuspended) {
	    synchronized(this) {
	      while (threadSuspended) {
		wait();
	      }
	    }
	  }
	  t.sleep(drawingSpeed); // interval given in milliseconds
	  drawInBuffer();
	  repaint();
	}

	t.sleep(3000);
      }
    }
    catch (InterruptedException e) { }
  }

  // burzy sciane miedzy dwoma komorkami labiryntu
  void knockDownWall(Coords newCell, Coords currentCell) {
    if (newCell.y < currentCell.y) { // sasiad na polnosc
      maze[currentCell.x][currentCell.y].walls.north=false;
      maze[newCell.x][newCell.y].walls.south=false;
    }
    else if (newCell.y > currentCell.y) { // poludnie
      maze[currentCell.x][currentCell.y].walls.south=false;
      maze[newCell.x][newCell.y].walls.north=false;
    }
    else if (newCell.x > currentCell.x) { // wschod
      maze[currentCell.x][currentCell.y].walls.east=false;
      maze[newCell.x][newCell.y].walls.west=false;
    }
    else if (newCell.x < currentCell.x) { // zachod
      maze[currentCell.x][currentCell.y].walls.west=false;
      maze[newCell.x][newCell.y].walls.east=false;
    }
  }

  public void drawInBuffer() {
    final int P_SIZ=6; // grubosc sciezek solution i backtrack
    buffer.clearRect(0, 0, width, height); // czyscimy bufor
    // rysowanie labiryntu, poprzez rysowanie jego kolejnych komorek
    buffer.setColor(Color.white);
    for (i=0; i < mazeWidth; i++)
      for (int j=0; j < mazeHeight; j++) { // rysowanie komorki
        if (maze[i][j].walls.north) // jesli stoi polnocna sciana
          buffer.drawLine(X_MAR+i*CELL_SIZE, Y_MAR+j*CELL_SIZE,
                 X_MAR+(i+1)*CELL_SIZE, Y_MAR+j*CELL_SIZE);
        if (maze[i][j].walls.south) // jesli stoi poludniowa sciana
          buffer.drawLine(X_MAR+i*CELL_SIZE, Y_MAR+(j+1)*CELL_SIZE,
                 X_MAR+(i+1)*CELL_SIZE, Y_MAR+(j+1)*CELL_SIZE);
	if (maze[i][j].walls.east) // jesli stoi wschodnia sciana
	    buffer.drawLine(X_MAR+(i+1)*CELL_SIZE, Y_MAR+j*CELL_SIZE,
		       X_MAR+(i+1)*CELL_SIZE, Y_MAR+(j+1)*CELL_SIZE);
	if (maze[i][j].walls.west) // jesli stoi zachodnia sciana
	    buffer.drawLine(X_MAR+i*CELL_SIZE, Y_MAR+j*CELL_SIZE,
		       X_MAR+i*CELL_SIZE, Y_MAR+(j+1)*CELL_SIZE);
    }
    // rysowanie backtrack
    buffer.setColor(new Color(96, 96, 255)); // niebieski
    for (i=0; i < mazeWidth; i++) {
      for (int j=0; j < mazeHeight; j++) { // rysowanie w komorce
        if (maze[i][j].backtrack.north)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+(j-1)*CELL_SIZE+P_SIZ,
			  CELL_SIZE-2*P_SIZ+1, 2*(CELL_SIZE-P_SIZ)+1);
        if (maze[i][j].backtrack.south)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  CELL_SIZE-2*P_SIZ+1, 2*(CELL_SIZE-P_SIZ)+1);
        if (maze[i][j].backtrack.east)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  2*(CELL_SIZE-P_SIZ)+1, CELL_SIZE-2*P_SIZ+1);
        if (maze[i][j].backtrack.west)
          buffer.fillRect(X_MAR+(i-1)*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  2*(CELL_SIZE-P_SIZ)+1, CELL_SIZE-2*P_SIZ+1);
      }
    }
    // rysowanie solution
    buffer.setColor(Color.yellow);
    for (i=0; i < mazeWidth; i++) {
      for (int j=0; j < mazeHeight; j++) { // rysowanie w komorce
        if (maze[i][j].solution.north)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+(j-1)*CELL_SIZE+P_SIZ,
			  CELL_SIZE-2*P_SIZ+1, 2*(CELL_SIZE-P_SIZ)+1);
        if (maze[i][j].solution.south)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  CELL_SIZE-2*P_SIZ+1, 2*(CELL_SIZE-P_SIZ)+1);
        if (maze[i][j].solution.east)
          buffer.fillRect(X_MAR+i*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  2*(CELL_SIZE-P_SIZ)+1, CELL_SIZE-2*P_SIZ+1);
        if (maze[i][j].solution.west)
          buffer.fillRect(X_MAR+(i-1)*CELL_SIZE+P_SIZ, Y_MAR+j*CELL_SIZE+P_SIZ,
			  2*(CELL_SIZE-P_SIZ)+1, CELL_SIZE-2*P_SIZ+1);
      }
    }
    // rysowanie wejscia i wyjscia z labiryntu
    if (entry != null) {
      buffer.setColor(Color.green);
      buffer.fillRect(X_MAR+CELL_SIZE*entry.x+2, Y_MAR+CELL_SIZE*entry.y+2,
	     CELL_SIZE-3, CELL_SIZE-3);
    }
      if (exit != null) {
      buffer.setColor(Color.red);
      buffer.fillRect(X_MAR+CELL_SIZE*exit.x+2, Y_MAR+CELL_SIZE*exit.y+2,
	     CELL_SIZE-3, CELL_SIZE-3);
    }
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync(); // ponoc polepsza wyglad w niektorych systemach
  }

  // metoda wykonywana przy ponownym wyrysowywaniu appleta
  public void paint(Graphics g) {
    update(g);
  }
}

/**
 * Stos utworzony przy pomocy listy dowiazaniowej
 * na podstawie Thinking In Java 4th Edition
 */
class CoordsStack<Coords> {
  private LinkedList<Coords> storage = new LinkedList<Coords>();
  public void push(Coords v) { storage.addFirst(v); }
  public Coords peek() { return storage.getFirst(); }
  public Coords pop() { return storage.removeFirst(); }
  public boolean empty() { return storage.isEmpty(); }
  public String toString() { return storage.toString(); }
}

class Coords {
  int x, y;
  Coords(int initX, int initY) {
    x=initX;
    y=initY;
  }
}

