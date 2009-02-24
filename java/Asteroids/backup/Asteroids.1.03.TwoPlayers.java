/*
  <applet width=640 height=480 code="Asteroids.class"></applet>
 */

import java.applet.Applet;
import java.lang.Thread;
import java.lang.Math;
import java.awt.Graphics;
import java.awt.Polygon;
import java.awt.Point;
import java.awt.Image;
import java.awt.Color;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.Random;
import java.util.Date;

/**
 * Gra Asteroids napisana w Javie.
 *
 * @author Micha³ Dettlaff
 * @version 1.3
 */
public class Asteroids extends Applet implements Runnable, KeyListener {
  final int STARS = 32;
  final int MAX_ASTEROIDS = 15;
  final int MIN_ASTEROIDS = 5;
  final int AWARD = 50;
  final int LIVES = 3;
  Random rand = new Random(new Date().getTime());
  Thread t;
  boolean threadSuspended;
  int width, height;
  Image bufferImage;
  Graphics buffer;
  int speed;
  boolean up, down, left, right;
  boolean fire;
  boolean p2up, p2down, p2left, p2right;
  boolean p2fire;
  boolean singlePlayer;
  boolean gameOver;
  boolean endGameScreen;
  int score;
  int p2score;
  int lives;
  int p2lives;
  Point[] stars;
  ArrayList<Asteroid> asteroids;
  ArrayList<Missile> missiles;
  Spaceship player;
  Spaceship player2;

  public void init() {
    t = null;
    width = getWidth();
    height = getHeight();
    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();

    gameOver = true;
    endGameScreen = false;
    singlePlayer = true;
    score = 0;
    lives = 0;
    player = new Spaceship(width/2, height/2, Color.WHITE, width, height);
    player2 = new Spaceship(width/2, height/2, Color.RED, width, height);
    speed = 40; // 1000/speed FPS
    stars = new Point[STARS];
    for (int i=0; i < stars.length; i++) {
      stars[i] = new Point(rand.nextInt(width), rand.nextInt(height));
    }
    asteroids = new ArrayList<Asteroid>();
    missiles = new ArrayList<Missile>();

    up = down = left = right = false;
    fire = false;
    p2up = p2down = p2left = p2right = false;
    p2fire = false;
    addKeyListener(this);
  }

  public void destroy() {
    removeKeyListener(this);
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
    threadSuspended = true;
  }

  void delay(int interval) {
    try {
      t.sleep(interval);
      if (threadSuspended) {
	synchronized(this) {
	  while (threadSuspended) {
	    wait();
	  }
	}
      }
    }
    catch (InterruptedException e) { }
  }

  public void run() {
    while (true) {
      repaintBuffer();
      while (gameOver);
      score = 0;
      lives = LIVES;
      player = new Spaceship(width/2, height/2, Color.WHITE, width, height);
      if (!singlePlayer) {
	player = new Spaceship(280, height/2, Color.CYAN, width, height);
	player2 = new Spaceship(360, height/2, Color.RED, width, height);
	p2score = 0;
	p2lives = LIVES;
      }
      missiles.clear();
      asteroids.clear();
      while (!gameOver) {
	repaintBuffer();
	if (singlePlayer) {
	  onePlayerGame();
	} else {
	  twoPlayerGame();
	}
	delay(speed);
      }
      repaintBuffer();
      while (endGameScreen);
    }
  }

  void onePlayerGame() {
    if (allNull(asteroids)) {
      asteroids.clear();
      for (int i=0; i < rand.nextInt(
	    MAX_ASTEROIDS - MIN_ASTEROIDS + 1) + MIN_ASTEROIDS; i++) {
	asteroids.add(new Asteroid(width, height));
      }
    }
    player.update(up, down, left, right);
    if (fire && player.reloaded() && player.immobility == 0) {
      missiles.add(new Missile(player));
      player.reloadMissile();
    }
    for (int i=0; i < missiles.size(); i++) {
      missiles.get(i).update();
      if (missiles.get(i).isGone()) { missiles.remove(i); }
    }
    for (int i=0; i < asteroids.size(); i++) {
      Asteroid a = asteroids.get(i);
      if (a != null) {
	a.update();
	if (a.color.equals(Color.BLACK)) {
	  asteroids.set(i, null);
	}
	if (player.isConflict(a) && !a.exploding) {
	  player1Destroyed();
	}
      }
    }
    for (Missile m : missiles) {
      if (player.contains(m.x, m.y) && player.invulnerability == 0) {
	player1Destroyed();
      }
    }
    for (Asteroid a : asteroids) {
      if (a != null) {
	for (int j=0; j < missiles.size(); j++) {
	  Missile m = missiles.get(j);
	  // czy pocisk trafil w asteroide
	  if (!a.exploding && a.contains(m.x, m.y)) {
	    missiles.remove(j);
	    a.explode();
	    score += AWARD;
	  }
	}
      }
    }
  }

  void twoPlayerGame() {
    if (allNull(asteroids)) {
      asteroids.clear();
      for (int i=0; i < rand.nextInt(
	    MAX_ASTEROIDS - MIN_ASTEROIDS + 1) + MIN_ASTEROIDS; i++) {
	asteroids.add(new Asteroid(width, height));
      }
    }
    player.update(up, down, left, right);
    if (fire && player.reloaded() && player.immobility == 0) {
      missiles.add(new Missile(player));
      player.reloadMissile();
    }
    player2.update(p2up, p2down, p2left, p2right);
    if (p2fire && player2.reloaded() && player2.immobility == 0) {
      missiles.add(new Missile(player2));
      player2.reloadMissile();
    }
    for (int i=0; i < missiles.size(); i++) {
      missiles.get(i).update();
      if (missiles.get(i).isGone()) { missiles.remove(i); }
    }
    for (int i=0; i < asteroids.size(); i++) {
      Asteroid a = asteroids.get(i);
      if (a != null) {
	a.update();
	if (a.color.equals(Color.BLACK)) {
	  asteroids.set(i, null);
	}
	if (player.isConflict(a) && !a.exploding) {
	  player1Destroyed();
	}
	if (player2.isConflict(a) && !a.exploding) {
	  player2Destroyed();
	}
	if (lives == -1 && p2lives == -1) {
	  gameOver = true;
	  endGameScreen = true;
	}
      }
    }
    for (Missile m : missiles) {
      if (player.contains(m.x, m.y) && player.invulnerability == 0) {
	player1Destroyed();
      }
      if (player2.contains(m.x, m.y) && player2.invulnerability == 0) {
	player2Destroyed();
      }
    }
    for (Asteroid a : asteroids) {
      if (a != null) {
	for (int j=0; j < missiles.size(); j++) {
	  Missile m = missiles.get(j);
	  // czy pocisk trafil w asteroide
	  if (!a.exploding && a.contains(m.x, m.y)) {
	    if (m.color.equals(player.color)) {
	      score += AWARD;
	    } else {
	      p2score += AWARD;
	    }
	    missiles.remove(j);
	    a.explode();
	  }
	}
      }
    }
  }

  void player1Destroyed() {
    if (singlePlayer) {
      if (lives == 0) {
	player.visible = false;
	gameOver = true;
	endGameScreen = true;
      } else if (lives != -1) {
	lives--;
	player = new Spaceship(width/2, height/2, Color.WHITE, width, height);
      }
    } else {
      if (lives == 0) {
	player.visible = false;
	player.immobilize();
	lives = -1;
      } else if (lives != -1) {
	lives--;
	player = new Spaceship(280, height/2, Color.CYAN, width, height);
      }
    }
  }

  void player2Destroyed() {
    if (p2lives == 0) {
      player2.visible = false;
      player2.immobilize();
      p2lives = -1;
    } else if (p2lives != -1) {
      p2lives--;
      player2 = new Spaceship(360, height/2, Color.RED, width, height);
    }
  }

  boolean allNull(ArrayList<Asteroid> asteroids) {
    for (Asteroid a : asteroids) {
      if (a != null) { return false; }
    }
    return true;
  }

  void repaintBuffer() {
    drawFrameInBuffer();
    repaint();
  }

  public void paint(Graphics g) {
    update(g);
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync();
  }

  void drawFrameInBuffer() {
    buffer.setColor(Color.BLACK);
    buffer.fillRect(0, 0, width, height);
    buffer.setColor(Color.WHITE);
    for (int i=0; i < stars.length; i++) {
      buffer.drawOval(stars[i].x, stars[i].y, 1, 1);
    }
    for (Asteroid a : asteroids) {
      if (a != null) {
	if (!a.exploding) {
	  buffer.setColor(Color.BLACK);
	  buffer.fillPolygon(a);
	}
	buffer.setColor(a.color);
	buffer.drawPolygon(a);
      }
    }
    if (player.visible && lives != -1) {
      buffer.setColor(Color.BLACK);
      buffer.fillPolygon(player);
      buffer.setColor(player.color);
      buffer.drawPolygon(player);
    }
    if (!singlePlayer && player2.visible && p2lives != -1) {
      buffer.setColor(Color.BLACK);
      buffer.fillPolygon(player2);
      buffer.setColor(player2.color);
      buffer.drawPolygon(player2);
    }
    for (Missile m : missiles) {
      buffer.setColor(m.color);
      buffer.fillOval((int) m.x-2, (int) m.y-2, 4, 4);
    }
    buffer.setColor(Color.WHITE);
    if (!gameOver || endGameScreen) {
      if (singlePlayer) {
	buffer.drawString("statki: " + lives, 12, 24);
      } else {
	if (lives != -1) {
	  buffer.drawString("statki gracza 1: " + lives, 12, 24);
	}
	if (p2lives != -1) {
	  buffer.drawString("statki gracza 2: " + p2lives, 12, 48);
	}
      }
      if (singlePlayer) {
	buffer.drawString("punkty: " + score, width - 120, 24);
      } else {
	buffer.drawString("punkty gracza 1: " + score, width - 170, 24);
	buffer.drawString("punkty gracza 2: " + p2score, width - 170, 48);
      }
    }
    if (endGameScreen) {
      buffer.drawString("Koniec gry!", width/2 - 30, height/2 - 10);
    }
    else if (gameOver) {
      buffer.drawString("ASTEROIDS", width/2 - 32, height/2 - 100);
      buffer.drawString("autor: Micha³ Dettlaff", width/2 - 55, height/2 - 55);
      buffer.drawString("1 - Jeden gracz", width/2 - 40, height/2);
      buffer.drawString("2 - Dwóch graczy", width/2 - 40, height/2 + 20);
      buffer.drawString("P - Pauza", width/2 - 40, height/2 + 50);
      buffer.drawString("Esc - Koniec gry", width/2 - 40, height/2 + 70);
    }
  }

  public void keyPressed(KeyEvent e) {
    switch (e.getKeyCode()) {
      case KeyEvent.VK_ESCAPE:
	gameOver = true;
	endGameScreen = false;
	break;
      case KeyEvent.VK_UP:
	up = true;
	break;
      case KeyEvent.VK_DOWN:
	down = true;
	break;
      case KeyEvent.VK_LEFT:
	left = true;
	break;
      case KeyEvent.VK_RIGHT:
	right = true;
	break;
      case KeyEvent.VK_SPACE:
	fire = true;
	break;
    }
    switch (e.getKeyChar()) {
      case 'w':
	p2up = true;
	break;
      case 's':
	p2down = true;
	break;
      case 'a':
	p2left = true;
	break;
      case 'd':
	p2right = true;
	break;
      case 'z':
	p2fire = true;
	break;
    }
  }

  public void keyReleased(KeyEvent e) {
    switch (e.getKeyCode()) {
      case KeyEvent.VK_UP:
	up = false;
	break;
      case KeyEvent.VK_DOWN:
	down = false;
	break;
      case KeyEvent.VK_LEFT:
	left = false;
	break;
      case KeyEvent.VK_RIGHT:
	right = false;
	break;
      case KeyEvent.VK_SPACE:
	fire = false;
	break;
    }
    switch (e.getKeyChar()) {
      case 'w':
	p2up = false;
	break;
      case 's':
	p2down = false;
	break;
      case 'a':
	p2left = false;
	break;
      case 'd':
	p2right = false;
	break;
      case 'z':
	p2fire = false;
	break;
    }
  }

  public void keyTyped(KeyEvent e) {
    switch (e.getKeyChar()) {
      case '1':
	if (gameOver) {
	  gameOver = false;
	  endGameScreen = false;
	  singlePlayer = true;
	}
	break;
      case '2':
	if (gameOver) {
	  gameOver = false;
	  endGameScreen = false;
	  singlePlayer = false;
	}
	break;
      case '\n':
	if (endGameScreen) {
	  endGameScreen = false;
	}
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
    }
  }

  public String getAppletInfo() {
    return "Gra Asteroids napisana w Javie.";
  }
}

class Spaceship extends Polygon {
  final int SIZE = 36;
  final double POWER = 2.0;
  final double MAX_SPEED = 12.0;
  final double HANDLING = 10.0;
  final int RELOAD_TIME = 8; // w klatkach
  double x, y;
  Color color;
  double speedX, speedY;
  double angle;
  int timeUntilReload;
  int width, height; // rozmiar planszy, po ktorej porusza sie statek
  int invulnerability;
  int immobility;
  boolean visible;

  Spaceship(int x, int y, Color color, int width, int height) {
    this.width = width;
    this.height = height;
    this.x = x;
    this.y = y;
    this.color = color;
    speedX = 0.0;
    speedY = 0.0;
    angle = 0.0;
    timeUntilReload = 0;
    invulnerability = 75;
    immobility = 25;
    visible = false;
  }

  void update(boolean up, boolean down, boolean left, boolean right) {
    double rad = Math.toRadians(angle);

    if (up && immobility == 0) { accelerate(); }
    if (down && immobility == 0) { decelerate(); }
    if (left && immobility == 0) { turnLeft(); }
    if (right && immobility == 0) { turnRight(); }
    // uwaga: os y rosnie w dol, a nie do gory
    x += speedX;
    y -= speedY;
    reset();
    // dziob, lewe skrzydlo, lewy tyl, prawy tyl, prawe skrzydlo
    addPoint((int) (x + SIZE/2 * Math.sin(rad)),
       	(int) (y - SIZE/2 * Math.cos(rad)));
    addPoint((int) (x + SIZE/2 * Math.sin(rad + 0.8 * Math.PI)),
       	(int) (y - SIZE/2 * Math.cos(rad + 0.8 * Math.PI)));
    addPoint((int) (x + SIZE/3 * Math.sin(rad + 0.85 * Math.PI)),
       	(int) (y - SIZE/3 * Math.cos(rad + 0.85 * Math.PI)));
    addPoint((int) (x + SIZE/3 * Math.sin(rad - 0.85 * Math.PI)),
       	(int) (y - SIZE/3 * Math.cos(rad - 0.85 * Math.PI)));
    addPoint((int) (x + SIZE/2 * Math.sin(rad - 0.8 * Math.PI)),
       	(int) (y - SIZE/2 * Math.cos(rad - 0.8 * Math.PI)));
    // zapewnia, ze statek nie wyleci poza plansze
    if (x < 0) { x = width; }
    if (x > width) { x = 0; }
    if (y < 0) { y = height; }
    if (y > height) { y = 0; }
    if (timeUntilReload > 0) {
      timeUntilReload--;
    }
    if (immobility > 0) { immobility--; }
    if (invulnerability > 0) {
      visible = !visible;
      invulnerability--;
    } else {
      visible = true;
    }
  }

  void accelerate() {
    double rad = Math.toRadians(angle);
    speedX += Math.sin(rad) * POWER;
    speedY += Math.cos(rad) * POWER;
    if (speedX > MAX_SPEED) { speedX = MAX_SPEED; }
    if (speedY > MAX_SPEED) { speedY = MAX_SPEED; }
    if (speedX < -MAX_SPEED) { speedX = -MAX_SPEED; }
    if (speedY < -MAX_SPEED) { speedY = -MAX_SPEED; }
  }

  void decelerate() {
    double rad = Math.toRadians(angle);
    speedX -= Math.sin(rad) * POWER;
    speedY -= Math.cos(rad) * POWER;
    if (speedX > MAX_SPEED) { speedX = MAX_SPEED; }
    if (speedY > MAX_SPEED) { speedY = MAX_SPEED; }
    if (speedX < -MAX_SPEED) { speedX = -MAX_SPEED; }
    if (speedY < -MAX_SPEED) { speedY = -MAX_SPEED; }
  }

  void turnLeft() {
    angle -= HANDLING;
  }

  void turnRight() {
    angle += HANDLING;
  }

  void reloadMissile() {
    timeUntilReload = RELOAD_TIME;
  }

  boolean reloaded() {
    return timeUntilReload == 0;
  }

  /** Czy pokrywa sie z innym wielokatem (np. asteroidem). */
  boolean isConflict(Polygon p) {
    if (invulnerability > 0) { return false; }
    // Dwa wielokaty pokrywaja sie, gdy co najmniej jeden wierzcholek
    // jednego z wielokatow zawiera sie w drugim wielokacie.
    for (int i=0; i < p.npoints; i++) {
      if (contains(p.xpoints[i], p.ypoints[i])) { return true; }
    }
    for (int i=0; i < npoints; i++) {
      if (p.contains(xpoints[i], ypoints[i])) { return true; }
    }
    return false;
  }

  void immobilize() {
    immobility = -1;
  }
}

class Missile {
  double speed;
  double x, y;
  Color color;
  double speedX, speedY;
  int width, height; // rozmiar planszy, po ktorej porusza sie pocisk

  Missile(Spaceship ss) {
    speed = ss.MAX_SPEED + 5.0;
    x = ss.x + ss.SIZE/2 * Math.sin(Math.toRadians(ss.angle));
    y = ss.y - ss.SIZE/2 * Math.cos(Math.toRadians(ss.angle));
    color = ss.color;
    speedX = speed * Math.sin(Math.toRadians(ss.angle));
    speedY = speed * Math.cos(Math.toRadians(ss.angle));
    this.width = ss.width;
    this.height = ss.height;
  }

  void update() {
    x += speedX;
    y -= speedY;
  }

  /** Czy pocisk wylecial poza plansze. */
  boolean isGone() {
    return x < 0 || x > width || y < 0 || y > height;
  }
}

class Asteroid extends Polygon {
  final int MAX_VERTICES = 10;
  final int SIZE = 50;
  static Random rand = new Random(new Date().getTime());
  double x, y;
  double angle;
  PointPolar[] shape;
  int vertices; // dla odroznienia od npoints
  double speedX, speedY;
  int width, height;
  double rotation;
  boolean exploding;
  Color color;

  Asteroid(int width, int height) {
    this.width = width;
    this.height = height;
    // asteroidy pojawiaja sie na obrzezach planszy
    x = rand.nextInt(width/2);
    if (x > width/4) { x += width*3/4; }
    y = rand.nextInt(height/2);
    if (y > height/4) { y += height*3/4; }
    angle = rand.nextInt(360);
    // predkosc musi byc rozna od zera
    speedX = rand.nextInt(3) + 1;
    if (rand.nextInt(2) == 0) { speedX = -speedX; }
    speedY = rand.nextInt(3) + 1;
    if (rand.nextInt(2) == 0) { speedY = -speedY; }
    rotation = rand.nextInt(17) - 8;

    vertices = rand.nextInt(MAX_VERTICES - 5 + 1) + 5;
    shape = new PointPolar[vertices];
    for (int i=0; i < vertices; i++) {
      int polarLength = rand.nextInt(SIZE - 16 + 1) + 16;
      int polarAngle = 360/vertices * i;
      shape[i] = new PointPolar(polarLength, polarAngle);
    }
    exploding = false;
    color = Color.WHITE;
  }

  void update() {
    double rad;

    x += speedX;
    y -= speedY;
    angle += rotation;
    rad = Math.toRadians(angle);
    reset();
    for (int i=0; i < vertices; i++) {
      double radShape = Math.toRadians(shape[i].angle);
      addPoint((int) (x + shape[i].length * Math.sin(rad + radShape)),
	  (int) (y - shape[i].length * Math.cos(rad + radShape)));
    }
    // zapewnia, ze obiekt nie wyleci poza plansze
    if (x < 0) { x = width; }
    if (x > width) { x = 0; }
    if (y < 0) { y = height; }
    if (y > height) { y = 0; }
    if (exploding) {
      color = color.darker();
    }
  }

  void explode() {
    exploding = true;
  }
}

/**
 * Przechowuje punkt okreslony przy pomocy wspolrzednych biegunowych.
 */
class PointPolar {
  int length;
  int angle;

  PointPolar(int length, int angle) {
    this.length = length;
    this.angle = angle;
  }

  public boolean equals(Object obj) {
    if (!(obj instanceof PointPolar)) {
      return false;
    }
    PointPolar pp = (PointPolar) obj;
    return (pp.length == length && pp.angle == angle);
  }
}

