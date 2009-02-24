/*
  <applet width=600 height=490 code="Biomorphs.class"></applet>
*/

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Random;
import java.util.Date;

/**
 * Biomorfy - ilustracja dzialania selekcji kumulatywnej. Na podstawie programu
 * Blind Watchmaker Richarda Dawkinsa.
 *
 * @author Micha³ Dettlaff
 * @version 1.1
 */
public class Biomorphs extends Applet {
  Controls controls;
  GenerationCanvas canvas;

  public void init() {
    setLayout(new BorderLayout());
    canvas = new GenerationCanvas();
    add(canvas, BorderLayout.CENTER);
    add(controls = new Controls(canvas), BorderLayout.SOUTH);
  }

  public void destroy() {
    remove(controls);
    remove(canvas);
  }

  public void start() {
    controls.setEnabled(true);
    canvas.setBuffer();
    canvas.paintBuffer();
  }

  public void stop() {
    controls.setEnabled(false);
  }

  public String getAppletInfo() {
    return "Biomorfy - ilustracja dzialania selekcji kumulatywnej.\nNa podstawie programu Blind Watchmaker Richarda Dawkinsa.";
  }
}

/**
 * Przechowuje genotyp biomorfa.
 */
class Biomorph {
  int angle;
  int elongation;
  //int expand;
  int iterations;
  double gradient;

  Biomorph(int INIT_ANGLE, int initElongation, int initIterations,
      double initGradient) {
    angle = INIT_ANGLE;
    elongation = initElongation;
    iterations = initIterations;
    gradient = initGradient;
  }

  /** Biomorf do kolejnego kroku rekursji. */
  Biomorph(Biomorph bm) {
    gradient = bm.gradient;
    angle = bm.angle;
    elongation = bm.elongation;

    iterations = bm.iterations-1;
    elongation = (int) (elongation * gradient);
  }

  protected Biomorph clone() {
    return new Biomorph(angle, elongation, iterations, gradient);
  }

  void incAngle() {
    angle += 10;
  }

  void decAngle() {
    angle -= 10;
  }

  void incElongation() {
    elongation += 2;
  }

  void decElongation() {
    elongation -= 2;
  }

  void incIteration() {
    iterations += 1;
  }

  void decIteration() {
    iterations -= 1;
  }

  void incGradient() {
    gradient += 0.1;
  }

  void decGradient() {
    gradient -= 0.1;
  }
}

class GenerationCanvas extends Canvas implements MouseListener {
  final int BM_WIDTH = 200;
  final int BM_HEIGHT = 150;
  int width, height;
  Graphics buffer;
  Image bufferImage;
  Biomorph parent;
  ArrayList<Biomorph> children;

  GenerationCanvas() {
    parent = getRandomBiomorph();
    children = new ArrayList<Biomorph>();
    createNewGeneration(parent);

    addMouseListener(this);
  }

  Biomorph getRandomBiomorph() {
    return new Biomorph(45, 30, 5, 0.8);
  }

  void createNewGeneration(Biomorph newParent) {
    Biomorph child;

    children.clear();
    parent = newParent;

    child = parent.clone();
    child.incAngle();
    children.add(child);
    child = parent.clone();
    child.decAngle();
    children.add(child);
    child = parent.clone();
    child.incElongation();
    children.add(child);
    child = parent.clone();
    child.decElongation();
    children.add(child);
    child = parent.clone();
    child.incIteration();
    children.add(child);
    child = parent.clone();
    child.decIteration();
    children.add(child);
    child = parent.clone();
    child.incGradient();
    children.add(child);
    child = parent.clone();
    child.decGradient();
    children.add(child);
  }

  public void setBuffer() {
    width = getWidth();
    height = getHeight();
    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();
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

  public void drawFrameInBuffer() {
    final int DY = 125;
    final int INIT_ANGLE = 90;

    buffer.clearRect(0, 0, width, height);
    drawBiomorph(buffer, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*0+DY),
	children.get(0), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*0+DY),
	children.get(1), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*0+DY),
	children.get(2), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*1+DY),
	children.get(3), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*1+DY),
	parent, INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*1+DY),
	children.get(4), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*0+BM_WIDTH/2, BM_HEIGHT*2+DY),
	children.get(5), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*1+BM_WIDTH/2, BM_HEIGHT*2+DY),
	children.get(6), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(BM_WIDTH*2+BM_WIDTH/2, BM_HEIGHT*2+DY),
	children.get(7), INIT_ANGLE);
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

  public void mouseReleased(MouseEvent e)  {
    int x = e.getX();
    int y = e.getY();

    if (y < BM_HEIGHT) {
      if (x < BM_WIDTH) {
        createNewGeneration(children.get(0));
      } else if (x < BM_WIDTH*2) {
        createNewGeneration(children.get(1));
      } else if (x < BM_WIDTH*3) {
        createNewGeneration(children.get(2));
      }
    } else if (y < BM_HEIGHT*2) {
      if (x < BM_WIDTH) {
        createNewGeneration(children.get(3));
      } else if (x < BM_WIDTH*2) {
	// kliknieto rodzica
      } else if (x < BM_WIDTH*3) {
        createNewGeneration(children.get(4));
      }
    } else if (y < BM_HEIGHT*3) {
      if (x < BM_WIDTH) {
        createNewGeneration(children.get(5));
      } else if (x < BM_WIDTH*2) {
        createNewGeneration(children.get(6));
      } else if (x < BM_WIDTH*3) {
        createNewGeneration(children.get(7));
      }
    }
    paintBuffer();
  }

  public void mouseClicked(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void mousePressed(MouseEvent e) {
  }
}

class Controls extends Panel implements ActionListener {
  GenerationCanvas canvas;
  Random rand;
  Date date;

  public Controls(GenerationCanvas canvas) {
    date = new Date();
    rand = new Random(date.getTime());
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
    if (label.equals("Od nowa")) {
      canvas.createNewGeneration(canvas.getRandomBiomorph());
    }
    if (label.equals("Losowo")) {
      canvas.createNewGeneration(canvas.children.get(
          rand.nextInt(canvas.children.size())));
    }
    canvas.paintBuffer();
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