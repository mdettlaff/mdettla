/*
  <applet width=750 height=640 code="Biomorphs.class"></applet>
*/

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Panel;
import java.awt.Button;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Random;
import java.util.Date;

/**
 * Biomorfy - ilustracja dzialania selekcji kumulatywnej. Na podstawie programu
 * Blind Watchmaker Richarda Dawkinsa.
 *
 * @author Micha³ Dettlaff
 * @version 1.3
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
    return "Biomorfy - ilustracja dzia³ania selekcji kumulatywnej.\nNa podstawie programu Blind Watchmaker Richarda Dawkinsa.";
  }
}

/**
 * Przechowuje genotyp biomorfa.
 */
class Biomorph {
  final int ANGLE_STEP = 18;
  final int MIN_ELONGATION = 2;
  final int MAX_ELONGATION = 32;
  final int ELONGATION_STEP = 2;
  final int MAX_ITERATIONS = 9;
  final double MAX_GRADIENT = 1.5;
  final double GRADIENT_STEP = 0.1;
  // geny
  int frontAngle;
  int rearAngle;
  int frontElongation;
  int rearElongation;
  //int expandX;
  //int expandY;
  int iterations;
  double gradient;

  Biomorph(int initFrontAngle, int initRearAngle, int initFrontElongation,
      int initRearElongation, int initIterations, double initGradient) {
    frontAngle = initFrontAngle;
    rearAngle = initRearAngle;
    frontElongation = initFrontElongation;
    rearElongation = initRearElongation;
    iterations = initIterations;
    gradient = initGradient;
  }

  /** Biomorf do kolejnego kroku rekursji. */
  Biomorph(Biomorph bm) {
    frontAngle = bm.frontAngle;
    rearAngle = bm.rearAngle;
    frontElongation = bm.frontElongation;
    rearElongation = bm.rearElongation;
    gradient = bm.gradient;

    iterations = bm.iterations-1;
    frontElongation = (int) (frontElongation * gradient);
    rearElongation = (int) (rearElongation * gradient);
  }

  protected Biomorph clone() {
    return new Biomorph(frontAngle, rearAngle, frontElongation, rearElongation,
	iterations, gradient);
  }

  void incFrontAngle() {
    frontAngle += ANGLE_STEP;
    if (frontAngle >= 360) {
      frontAngle -= 360;
    }
  }

  void decFrontAngle() {
    frontAngle -= ANGLE_STEP;
    if (frontAngle < 0) {
      frontAngle += 360;
    }
  }

  void incRearAngle() {
    rearAngle += ANGLE_STEP;
    if (rearAngle >= 360) {
      rearAngle -= 360;
    }
  }

  void decRearAngle() {
    rearAngle -= ANGLE_STEP;
    if (rearAngle < 0) {
      rearAngle += 360;
    }
  }

  void incFrontElongation() {
    frontElongation += ELONGATION_STEP;
    if (frontElongation > MAX_ELONGATION) {
      frontElongation = MAX_ELONGATION;
    }
  }

  void decFrontElongation() {
    frontElongation -= ELONGATION_STEP;
    if (frontElongation < MIN_ELONGATION) {
      frontElongation = MIN_ELONGATION;
    }
  }

  void incRearElongation() {
    rearElongation += ELONGATION_STEP;
    if (rearElongation > MAX_ELONGATION) {
      rearElongation = MAX_ELONGATION;
    }
  }

  void decRearElongation() {
    rearElongation -= ELONGATION_STEP;
    if (rearElongation < MIN_ELONGATION) {
      rearElongation = MIN_ELONGATION;
    }
  }

  void incIteration() {
    iterations += 1;
    if (iterations > MAX_ITERATIONS) {
      iterations = MAX_ITERATIONS;
    }
  }

  void decIteration() {
    iterations -= 1;
    if (iterations < 0) {
      iterations = 0;
    }
  }

  void incGradient() {
    gradient += GRADIENT_STEP;
    if (gradient > MAX_GRADIENT) {
      gradient = MAX_GRADIENT;
    }
  }

  void decGradient() {
    gradient -= GRADIENT_STEP;
    if (gradient <= 0) {
      gradient += GRADIENT_STEP;
    }
  }
}

class GenerationCanvas extends Canvas implements MouseListener {
  final Biomorph INIT_PARENT = new Biomorph(45, 45, 20, 20, 5, 0.85);
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
    return INIT_PARENT;
  }

  void createNewGeneration(Biomorph newParent) {
    Biomorph child;

    children.clear();
    parent = newParent;

    child = parent.clone();
    child.incFrontAngle();
    children.add(child);

    child = parent.clone();
    child.decFrontAngle();
    children.add(child);

    child = parent.clone();
    child.incRearAngle();
    children.add(child);

    child = parent.clone();
    child.decRearAngle();
    children.add(child);

    child = parent.clone();
    child.incFrontElongation();
    children.add(child);

    child = parent.clone();
    child.decFrontElongation();
    children.add(child);

    child = parent.clone();
    child.incRearElongation();
    children.add(child);

    child = parent.clone();
    child.decRearElongation();
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
    drawBuffer();
    repaint();
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync(); // ponoc polepsza wyglad w niektorych systemach
  }

  public void drawBuffer() {
    final int DY = 70;
    final int INIT_ANGLE = 0; // 0 - biomorfy skierowane w gore
    Iterator<Biomorph> childIter = children.iterator();

    buffer.clearRect(0, 0, width, height);
    drawBiomorph(buffer, new Coords(width/5*0+width/5/2, height/4*0+DY),
	parent, INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*1+width/5/2, height/4*0+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*2+width/5/2, height/4*0+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*3+width/5/2, height/4*0+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*4+width/5/2, height/4*0+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*0+width/5/2, height/4*1+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*1+width/5/2, height/4*1+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*2+width/5/2, height/4*1+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*3+width/5/2, height/4*1+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*4+width/5/2, height/4*1+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*0+width/5/2, height/4*2+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*1+width/5/2, height/4*2+DY),
	childIter.next(), INIT_ANGLE);
    drawBiomorph(buffer, new Coords(width/5*2+width/5/2, height/4*2+DY),
	childIter.next(), INIT_ANGLE);
  }
 
  void drawBiomorph(Graphics g, Coords cursor, Biomorph bm, int angle) {
    if (angle >= 360) { // zakres kata od 0 do 359 stopni
      angle -= 360;
    } else if (angle < 0) {
      angle += 360;
    }
    if (angle >= 270 || angle <= 90) { // kierunek do gory
      cursor = drawLineAngle(g, cursor, angle, bm.frontElongation);
    } else { // kierunek do dolu
      cursor = drawLineAngle(g, cursor, angle, bm.rearElongation);
    }
    if (bm.iterations > 0) {
      if (angle == 0) { // kat zero stopni to kierunek do gory
	drawBiomorph(g, cursor, new Biomorph(bm), 0 + bm.frontAngle);
	drawBiomorph(g, cursor, new Biomorph(bm), 0 - bm.frontAngle);
      } else if (angle < 90) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0);
	drawBiomorph(g, cursor, new Biomorph(bm), 90);
      } else if (angle == 90) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0 + bm.frontAngle);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 - bm.rearAngle);
      } else if (angle < 180) {
	drawBiomorph(g, cursor, new Biomorph(bm), 90);
	drawBiomorph(g, cursor, new Biomorph(bm), 180);
      } else if (angle == 180) {
	drawBiomorph(g, cursor, new Biomorph(bm), 180 + bm.rearAngle);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 - bm.rearAngle);
      } else if (angle < 270) {
	drawBiomorph(g, cursor, new Biomorph(bm), 180);
	drawBiomorph(g, cursor, new Biomorph(bm), 270);
      } else if (angle == 270) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0 - bm.frontAngle);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 + bm.rearAngle);
      } else if (angle < 360) {
	drawBiomorph(g, cursor, new Biomorph(bm), 270);
	drawBiomorph(g, cursor, new Biomorph(bm), 0);
      }
    }
  }

  Coords drawLineAngle(Graphics g, Coords start, int angle, int len) {
    Coords dest = new Coords(start.x, start.y);
    double angleD, lenD;

    angleD = angle + 90; // przyjmujemy, ze 0 stopni to kierunek do gory
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

    if (y < height/4*1) {
      if (x < width/5*1) {
        createNewGeneration(parent);
      } else if (x < width/5*2) {
        createNewGeneration(children.get(0));
      } else if (x < width/5*3) {
        createNewGeneration(children.get(1));
      } else if (x < width/5*4) {
        createNewGeneration(children.get(2));
      } else if (x < width/5*5) {
        createNewGeneration(children.get(3));
      }
    } else if (y < height/4*2) {
      if (x < width/5*1) {
        createNewGeneration(children.get(4));
      } else if (x < width/5*2) {
        createNewGeneration(children.get(5));
      } else if (x < width/5*3) {
        createNewGeneration(children.get(6));
      } else if (x < width/5*4) {
        createNewGeneration(children.get(7));
      } else if (x < width/5*5) {
        createNewGeneration(children.get(8));
      }
    } else if (y < height/4*3) {
      if (x < width/5*1) {
        createNewGeneration(children.get(9));
      } else if (x < width/5*2) {
        createNewGeneration(children.get(10));
      } else if (x < width/5*3) {
        createNewGeneration(children.get(11));
      } else if (x < width/5*4) {
      } else if (x < width/5*5) {
      }
    } else if (y < height/4*4) {
      if (x < width/5*1) {
      } else if (x < width/5*2) {
      } else if (x < width/5*3) {
      } else if (x < width/5*4) {
      } else if (x < width/5*5) {
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
    b = new Button("Restart");
    b.addActionListener(this);
    add(b);
    b = new Button("Losuj potomka");
    b.addActionListener(this);
    add(b);
    b = new Button("Przetasuj");
    b.addActionListener(this);
    add(b);
  }

  public void actionPerformed(ActionEvent e) {
    String label = e.getActionCommand();
    if (label.equals("Restart")) {
      canvas.createNewGeneration(canvas.INIT_PARENT);
    }
    if (label.equals("Losuj potomka")) {
      canvas.createNewGeneration(canvas.children.get(
          rand.nextInt(canvas.children.size())));
    }
    if (label.equals("Przetasuj")) {
      canvas.createNewGeneration(canvas.getRandomBiomorph());
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

  public boolean equals(Object obj) {
   if (!(obj instanceof Coords)) {
     return false;
   }
   Coords coords = (Coords) obj;
   return (coords.x == x && coords.y == y);
  }

  static Coords add(Coords c1, Coords c2) {
    return new Coords(c1.x + c2.x, c1.y + c2.y);
  }
}

