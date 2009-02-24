/*
  <applet width=640 height=480 code="Asteroids.class"></applet>
 */

import java.applet.Applet;
import java.lang.Thread;
import java.lang.Math;
import java.awt.Graphics;
import java.awt.Polygon;
import java.awt.Image;
import java.awt.Color;
import java.awt.event.KeyListener;
import java.awt.event.KeyEvent;

/**
 * Gra Asteroids napisana w Javie.
 *
 * @author Micha³ Dettlaff
 * @version 0.2
 */
public class Asteroids extends Applet implements Runnable, KeyListener {
  Thread t;
  boolean threadSuspended;
  int width, height;
  Image bufferImage;
  Graphics buffer;
  int speed;
  Starship player;

  public void init() {
    t = null;
    width = getWidth();
    height = getHeight();
    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();

    speed = 40; // 25 klatek na sekunde
    player = new Starship(width/2, height/2);

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
      player.update();
      delay(speed);
    }
  }

  public void paint(Graphics g) {
    update(g);
  }

  void repaintBuffer() {
    drawFrameInBuffer();
    repaint();
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync();
  }

  void drawFrameInBuffer() {
    buffer.clearRect(0, 0, width, height);
    buffer.setColor(Color.BLACK);
    buffer.drawPolygon(player);
  }

  public void keyPressed(KeyEvent e) {
    switch (e.getKeyCode()) {
      case KeyEvent.VK_UP:
	player.accelerate();
	break;
      case KeyEvent.VK_DOWN:
	player.decelerate();
	break;
      case KeyEvent.VK_LEFT:
	player.turnLeft();
	break;
      case KeyEvent.VK_RIGHT:
	player.turnRight();
	break;
    }
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
  }

  public String getAppletInfo() {
    return "Gra Asteroids napisana w Javie.";
  }
}

class Starship extends Polygon {
  final int SIZE = 22;
  final double POWER = 0.1;
  final double HANDLING = 5.0;
  double x, y;
  double speed;
  double angle;

  Starship(int x, int y) {
    this.x = x;
    this.y = y;
    speed = 0.0;
    angle = 0.0;
    update();
  }

  void update() {
    double rad = Math.toRadians(angle);

    // uwaga: os y rosnie w dol, a nie do gory
    x += speed * Math.sin(rad);
    y -= speed * Math.cos(rad);
    reset();
    // dziob, lewe skrzydlo, ogon, prawe skrzydlo
    addPoint((int) (x + SIZE * Math.sin(rad)),
       	(int) (y - SIZE * Math.cos(rad)));
    addPoint((int) (x + SIZE/2 * Math.sin(rad + 0.7 * Math.PI)),
       	(int) (y - SIZE/2 * Math.cos(rad + 0.7 * Math.PI)));
    addPoint((int) x, (int) y);
    addPoint((int) (x + SIZE/2 * Math.sin(rad - 0.7 * Math.PI)),
       	(int) (y - SIZE/2 * Math.cos(rad - 0.7 * Math.PI)));
  }

  void accelerate() {
    speed += POWER;
  }

  void decelerate() {
    speed -= POWER;
  }

  void turnLeft() {
    if (speed >= 0) { angle -= HANDLING; }
    else { angle += HANDLING; }
  }

  void turnRight() {
    if (speed >= 0) { angle += HANDLING; }
    else { angle -= HANDLING; }
  }
}

