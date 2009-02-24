/*
  <applet width=320 height=200 code="Asteroids.class"></applet>
 */

import java.applet.Applet;
import java.lang.Thread;
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
    speed = 100;
    player = new Starship();

    width = getWidth();
    height = getHeight();
    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();

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
      player.move();
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

  public void keyTyped(KeyEvent e) {
    switch (e.getKeyChar()) {
      case 'w':
      case 'W':
	player.moveUp();
	break;
      case 's':
      case 'S':
	player.moveDown();
	break;
      case 'a':
      case 'A':
	player.moveLeft();
	break;
      case 'd':
      case 'D':
	player.moveRight();
	break;
    }
  }

  public void keyPressed(KeyEvent e) {
  }

  public void keyReleased(KeyEvent e) {
  }

  public String getAppletInfo() {
    return "Gra Asteroids napisana w Javie.";
  }
}

class Starship extends Polygon {
  double x, y;
  double speedX, speedY;
  int angle;
  int speed;

  Starship() {
    npoints = 4;
    xpoints = new int[npoints];
    ypoints = new int[npoints];
    angle = 90;
    speedX = 0;
    speedY = 0;
    x = 50.0;
    y = 50.0;
    updateShape();
  }

  void updateShape() {
    // dziob, lewe skrzydlo, ogon, prawe skrzydlo
    xpoints[0] = (int) x;
    ypoints[0] = (int) y - 15;
    xpoints[1] = (int) x - 10;
    ypoints[1] = (int) y + 10;
    xpoints[2] = (int) x;
    ypoints[2] = (int) y;
    xpoints[3] = (int) x + 10;
    ypoints[3] = (int) y + 10;
  }

  void moveUp() {
    speedY -= 0.1;
  }

  void moveDown() {
    speedY += 0.1;
  }

  void moveLeft() {
    speedX -= 0.1;
  }

  void moveRight() {
    speedX += 0.1;
  }

  void move() {
    x += speedX;
    y += speedY;
    updateShape();
  }
}

