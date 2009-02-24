/*
  <applet width=600 height=500
      code="Biomorphs.class">
  </applet>
*/

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.Math;

/**
 * Biomorfy - ilustracja selekcji kumulatywnej. Na podstawie ksiazki "The Blind
 * Watchmaker" Richarda Dawkinsa.
 *
 * @author Micha³ Dettlaff
 * @version 0.1
 */
public class Biomorphs extends Applet {
  Controls controls;
  GenerationCanvas canvas;

  public void init() {
    setLayout(new BorderLayout());
    canvas = new GenerationCanvas();
    add("Center", canvas);
    add("South", controls = new Controls(canvas));
  }

  public void destroy() {
    remove(controls);
    remove(canvas);
  }

  public void start() {
    controls.setEnabled(true);
  }

  public void stop() {
    controls.setEnabled(false);
  }

  public String getAppletInfo() {
    return "Biomorfy - ilustracja selekcji kumulatywnej.\nNa podstawie \"The Blind Watchmaker\" Richarda Dawkinsa.";
  }
}

class GenerationCanvas extends Canvas {

  public void paint(Graphics g) {
    drawBiomorph(g, new Coords(100, 140), 1, 90, 30);
    drawBiomorph(g, new Coords(300, 140), 2, 90, 30);
    drawBiomorph(g, new Coords(500, 140), 3, 90, 30);
    drawBiomorph(g, new Coords(100, 290), 4, 90, 30);
    drawBiomorph(g, new Coords(300, 290), 5, 90, 30);
    drawBiomorph(g, new Coords(500, 290), 6, 90, 30);
    drawBiomorph(g, new Coords(100, 440), 7, 90, 30);
    drawBiomorph(g, new Coords(300, 440), 8, 90, 30);
    drawBiomorph(g, new Coords(500, 440), 9, 90, 30);
  }
  
  void drawBiomorph(Graphics g, Coords cursor, int depth,
		      int arc, int len) {
    cursor = drawLineArc(g, cursor, arc, len);
    if (depth > 0 && len > 0) {
      drawBiomorph(g, cursor, depth-1, arc+45, (int) (len * 0.8));
      drawBiomorph(g, cursor, depth-1, arc-45, (int) (len * 0.8));
    }
  }

  Coords drawLineArc(Graphics g, Coords cursor, int arc, int len) {
    int x2 = cursor.x;
    int y2 = cursor.y;
    double arcD, lenD;

    arcD = arc;
    arcD = Math.toRadians(arcD);
    lenD = len;
    x2 += (int) (lenD * Math.cos(arcD));
    y2 += (int) (lenD * Math.sin(arcD) * (-1));
    g.drawLine(cursor.x, cursor.y, x2, y2);

    return new Coords(x2, y2);
  }

  public void redraw() {
    repaint();
  }
}

class Controls extends Panel
          implements ActionListener {
  GenerationCanvas canvas;

  public Controls(GenerationCanvas canvas) {
    Button b = null;

    this.canvas = canvas;
    b = new Button("Od nowa");
    b.addActionListener(this);
    add(b);
    b = new Button("Losowo");
    b.addActionListener(this);
    add(b);
  }

  public void actionPerformed(ActionEvent ev) {
    String label = ev.getActionCommand();

    canvas.redraw();
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