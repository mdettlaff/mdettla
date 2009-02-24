// tag dla appletviewera; otworz komenda appletviewer Maze.java
// <applet width=385 height=385 code="Maze.class"></applet>

import java.applet.*;
import java.awt.*;
import java.util.LinkedList;
import java.util.Random;
import java.util.Date;
 
// nazwa klasy musi byc identyczna z nazwa pliku
public class Maze extends Applet implements Runnable {
 
    // klasa wewnetrzna reprezentujaca komorke labiryntu
    private class MazeCell {
	private class Directions {
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
    }

    Thread t = null;
    boolean threadSuspended;
    int i;
    int width, height;
    int cellSize=15; // wielkosc komorki labiryntu w pikselach
    int mazeWidth=25; // szerokosc labiryntu w komorkach
    int mazeHeight=25; // wysokosc labiryntu w komorkach
    int xMar=5; // margines boczny
    int yMar=5; // margines gorny i dolny
    Color backgroundColor = new Color(96, 96, 96); // ciemnoszary
    // tablica reprezentujaca labirynt w postaci komorek
    MazeCell[][] maze = new MazeCell[mazeWidth][mazeHeight];
    Coords start = new Coords(0, mazeHeight-1); // wspolrzedne startu labiryntu
    Coords finish = new Coords(mazeWidth-1, 0); // wspolrzedne konca labiryntu

    // metoda wykonuje sie przy uruchomieniu appleta
    public void init() {
	// szerokosc i wysokosc appletu
	width = getSize().width;
	height = getSize().height;
	setBackground(backgroundColor);

	// Inicjalizujemy labirynt
	// najpierw wstawiamy wszedzie sciany
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
	    /* Budujemy labirynt przez sukcesywne burzenie scian, stosujac
	     * Algorytm Depth-First Search generowania labiryntu
	     * algorytm tworzy labirynt typu perfect maze, tzn. taki, w ktorym
	     * istnieje dokladnie jedna sciezka miedzy kazdymi dwoma pozycjami */
	    CoordsStack<Coords> cellStack = new CoordsStack<Coords>();
	    int totalCells = mazeWidth * mazeHeight;
	    Date date = new Date(); // czas uniksowy wykorzystamy jako seed w rand
	    Random rand = new Random(date.getTime()); // losowy generator liczb
	    Coords currentCell = new Coords(rand.nextInt(mazeWidth),
					    rand.nextInt(mazeHeight));
	    int visitedCells = 1;
	      
	    while (visitedCells < totalCells) {
		// find all neighbors of currentCell with all walls intact
		// pozycje sasiednich komorek (polnoc, poludnie, wschod, zachod)
		Coords[] directions = {new Coords(0,-1), new Coords(0,1),
				       new Coords(1,0), new Coords(-1,0)};
		Coords[] intactNeighbors = new Coords[4];
		int i, intactNborsCount; // number of intact neighbors
		for (i=0, intactNborsCount=0; i < 4; i++) {
		    int x=directions[i].x+currentCell.x;
		    int y=directions[i].y+currentCell.y;
		    if (!isBorderInDirection(currentCell, directions[i]))
			if (isIntact(maze[x][y])) {
			    intactNeighbors[intactNborsCount] = new Coords(x, y);
			    intactNborsCount++;
			}
		}
		if (intactNborsCount > 0) { // if one or more found 
		    int randInd=rand.nextInt(intactNborsCount); // random index
		    Coords newCell = new Coords(intactNeighbors[randInd].x,
						intactNeighbors[randInd].y);
		    // knock down the wall between it and CurrentCell 
		    knockDownWall(newCell, currentCell);

		    // Now the thread checks to see if it should suspend itself
		    if (threadSuspended) {
		       synchronized( this ) {
			  while (threadSuspended) {
			     wait();
			  }
		       }
		    }
		    repaint();
		    t.sleep(25);  // interval given in milliseconds

		    cellStack.push(currentCell);
		    currentCell=newCell;
		    visitedCells++;
		}
		else 
		    currentCell=cellStack.pop();
	    }
	}
	catch (InterruptedException e) { }
    }

    // sprawdza, czy jest brzeg w danym miejscu komorki
    private boolean isBorderInDirection(Coords cell, Coords direction) {
	if (direction.y < 0 && maze[cell.x][cell.y].border.north == true)
	    return true; // jest brzeg na polnoc
	if (direction.y > 0 && maze[cell.x][cell.y].border.south == true)
	    return true; // jest brzeg na poludnie
	if (direction.x > 0 && maze[cell.x][cell.y].border.east == true)
	    return true; // jest brzeg na wschod
	if (direction.x < 0 && maze[cell.x][cell.y].border.west == true)
	    return true; // jest brzeg na zachod
	return false;
    }

    // burzy sciane miedzy dwoma komorkami labiryntu
    private void knockDownWall(Coords newCell, Coords currentCell) {
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

    // sprawdza, czy wszystkie sciany komorki stoja
    private boolean isIntact(MazeCell c) {
	if (c.walls.north && c.walls.south && c.walls.east && c.walls.west)
	    return true;
	else
	    return false;
    }
 
    // metoda wykonywana przy ponownym wyrysowywaniu appleta
    public void paint(Graphics g) {
	// kolor linii
	g.setColor(Color.white);
	// rysowanie labiryntu, poprzez rysowanie jego kolejnych komorek
	for (int i=0; i < mazeWidth; i++)
	    for (int j=0; j < mazeWidth; j++) { // rysowanie komorki
		if (maze[i][j].walls.north) // jesli stoi polnocna sciana
		    g.drawLine(xMar+i*cellSize, yMar+j*cellSize,
			       xMar+(i+1)*cellSize, yMar+j*cellSize);
		if (maze[i][j].walls.south) // jesli stoi poludniowa sciana
		    g.drawLine(xMar+i*cellSize, yMar+(j+1)*cellSize,
			       xMar+(i+1)*cellSize, yMar+(j+1)*cellSize);
		if (maze[i][j].walls.east) // jesli stoi wschodnia sciana
		    g.drawLine(xMar+(i+1)*cellSize, yMar+j*cellSize,
			       xMar+(i+1)*cellSize, yMar+(j+1)*cellSize);
		if (maze[i][j].walls.west) // jesli stoi zachodnia sciana
		    g.drawLine(xMar+i*cellSize, yMar+j*cellSize,
			       xMar+i*cellSize, yMar+(j+1)*cellSize);
	    }
	// rysujemy poczatek i punkt docelowy
	g.setColor(Color.green);
	g.fillRect(xMar+cellSize*start.x+2, yMar+cellSize*start.y+2,
	       	   cellSize-3, cellSize-3);
	g.setColor(Color.red);
	g.fillRect(xMar+cellSize*finish.x+2, yMar+cellSize*finish.y+2,
	           cellSize-3, cellSize-3);
	g.setColor(Color.white);
    }
}

// tworzymy stos korzystajac z listy dowiazaniowej;
// na podstawie Thinking In Java 4th Edition
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

