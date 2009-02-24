/*
  <applet width=600 height=490
      code="Biomorphs.class">
  </applet>
*/

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.Math;

/**
 * Biomorfy - ilustracja selekcji kumulatywnej. Na podstawie "The Blind
 * Watchmaker" Richarda Dawkinsa.
 *
 * @author Micha³ Dettlaff
 * @version 0.2
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

/**
 * Przechowuje genotyp biomorfa.
 */
class Biomorph {
  int angle;
  int elongation;
  int iterations;
  double gradient;

  Biomorph(int initAngle, int initElongation, int initIterations,
      double initGradient) {
    elongation = initElongation;
    angle = initAngle;
    gradient = initGradient;
    iterations = initIterations;
  }

  /** Biomorf do kolejnego kroku rekursji. */
  Biomorph(Biomorph bm) {
    gradient = bm.gradient;
    angle = bm.angle;
    elongation = bm.elongation;

    iterations = bm.iterations-1;
    elongation = (int) (elongation * gradient);
  }
}

class GenerationCanvas extends Canvas {
  final int BM_WIDTH = 200;
  final int BM_HEIGHT = 150;

  public void paint(Graphics g) {
    drawBiomorph(g, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*0+140),
       	new Biomorph(45, 30, 0, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*0+140),
       	new Biomorph(45, 30, 1, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*0+140),
       	new Biomorph(45, 30, 3, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*1+140),
       	new Biomorph(45, 30, 4, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*1+140),
       	new Biomorph(45, 30, 5, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*1+140),
       	new Biomorph(45, 30, 7, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*2+140),
       	new Biomorph(65, 30, 8, 0.7), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*2+140),
       	new Biomorph(45, 30, 9, 0.8), 90);
    drawBiomorph(g, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*2+140),
       	new Biomorph(45, 30, 10, 0.8), 90);
  }
 
  void drawBiomorph(Graphics g, Coords cursor, Biomorph bm, int angle) {
    cursor = drawLineAngle(g, cursor, angle, bm.elongation);
    if (bm.iterations > 0 && bm.elongation > 0) {
      drawBiomorph(g, cursor, new Biomorph(bm), angle+bm.angle);
      drawBiomorph(g, cursor, new Biomorph(bm), angle-bm.angle);
    }
  }

  Coords drawLineAngle(Graphics g, Coords start, int angle, int len) {
    Coords dest = new Coords(start.x, start.y);
    double angleD, lenD;

    angleD = angle;
    angleD = Math.toRadians(angleD);
    lenD = len;
    dest.x += (int) (lenD * Math.cos(angleD));
    dest.y += (int) (lenD * Math.sin(angleD) * (-1));
    g.drawLine(start.x, start.y, dest.x, dest.y);

    return dest;
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
    x = initX;
    y = initY;
  }

  boolean equals(Coords coords) {
    return (coords.x == x && coords.y == y);
  }

  static Coords add(Coords c1, Coords c2) {
    return new Coords(c1.x + c2.x, c1.y + c2.y);
  }
}

