/*
  <applet width=750 height=600 code="Biomorphs.class"></applet>
*/

import java.applet.Applet;
import java.awt.BorderLayout;
import java.awt.Canvas;
import java.awt.Panel;
import java.awt.Button;
import java.awt.Checkbox;
import java.awt.Label;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.Color;
import java.awt.event.MouseListener;
import java.awt.event.MouseEvent;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;
import java.awt.event.ItemListener;
import java.awt.event.ItemEvent;
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
 * @version 1.6
 */
public class Biomorphs extends Applet {
  Controls controls;
  GenerationCanvas canvas;
  static Random rand = new Random(new Date().getTime());

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
  static final int ANGLE_STEP = 15;
  static final int MIN_ELONGATION = 0;
  static final int MAX_ELONGATION = 90;
  static final int ELONGATION_STEP = 5;
  static final double MIN_EXPAND = 0.0;
  static final double MAX_EXPAND = 2.5;
  static final double EXPAND_STEP = 0.1;
  static final int MAX_ITERATIONS = 9;
  static final double MAX_GRADIENT = 1.5;
  static final double GRADIENT_STEP = 0.1;
  static final double COLOR_STEP = 0.1;
  static final Color INIT_COLOR = new Color(96, 96, 255);
  // geny
  int frontAngle;
  int rearAngle;
  int frontElongation;
  int rearElongation;
  double expandX;
  double expandY;
  int iterations;
  double gradient;
  Color color;
  String shape;

  Biomorph(int initFrontAngle, int initRearAngle, int initFrontElongation,
      int initRearElongation, double initExpandX, double initExpandY,
      int initIterations, double initGradient, Color initColor,
      String initShape) {
    frontAngle = initFrontAngle;
    rearAngle = initRearAngle;
    frontElongation = initFrontElongation;
    rearElongation = initRearElongation;
    expandX = initExpandX;
    expandY = initExpandY;
    iterations = initIterations;
    gradient = initGradient;
    color = initColor;
    shape = initShape;
  }

  /** Biomorf do kolejnego kroku rekursji. */
  Biomorph(Biomorph bm) {
    frontAngle = bm.frontAngle;
    rearAngle = bm.rearAngle;
    frontElongation = bm.frontElongation;
    rearElongation = bm.rearElongation;
    expandX = bm.expandX;
    expandY = bm.expandY;
    gradient = bm.gradient;
    shape = bm.shape;

    if (iterations < 7) { color = darker(bm.color); }
    iterations = bm.iterations-1;
    frontElongation = (int) (frontElongation * gradient);
    rearElongation = (int) (rearElongation * gradient);
  }

  Color darker(Color color) {
    int r=color.getRed();
    int g=color.getGreen();
    int b=color.getBlue();
    r-=16; g-=16; b-=16;
    if (r < 0) { r=0; }
    if (g < 0) { g=0; }
    if (b < 0) { b=0; }
    return new Color(r, g, b);
  }

  protected Biomorph clone() {
    return new Biomorph(frontAngle, rearAngle, frontElongation, rearElongation,
	expandX, expandY, iterations, gradient, color, shape);
  }

  static final Biomorph getDefault() {
    return new Biomorph(45, 45, 15, 15, 1.0, 1.0, 5, 0.8,
       	Biomorph.INIT_COLOR, "line");
  }

  static Biomorph getRandom() {
    int frontAngle = Biomorphs.rand.nextInt(360/ANGLE_STEP)*ANGLE_STEP;
    int rearAngle = Biomorphs.rand.nextInt(360/ANGLE_STEP)*ANGLE_STEP;
    int frontElongation = Biomorphs.rand.nextInt(
	    MAX_ELONGATION/ELONGATION_STEP)*ELONGATION_STEP;
    int rearElongation = Biomorphs.rand.nextInt(
	    MAX_ELONGATION/ELONGATION_STEP)*ELONGATION_STEP;
    double expandX = ((double) Biomorphs.rand.nextInt((int)(MAX_EXPAND*10)))
							/ 10;
    double expandY = ((double) Biomorphs.rand.nextInt((int)(MAX_EXPAND*10)))
							/ 10;
    int iterations = Biomorphs.rand.nextInt(MAX_ITERATIONS+1);
    double gradient = ((double) Biomorphs.rand.nextInt((int)(MAX_GRADIENT*10))
							/ 10);
    Color color = randomBiomorphColor(Biomorphs.rand);
    String shape;
    switch (Biomorphs.rand.nextInt(3)) {
      case 0: shape = "line"; break;
      case 1: shape = "rect"; break;
      case 2: shape = "oval"; break;
      default: shape = "line"; break;
    }
    return new Biomorph(frontAngle, rearAngle, frontElongation, rearElongation,
	expandX, expandY, iterations, gradient, color, shape);
  }

  static Color randomBiomorphColor(Random rand) {
    Color color = INIT_COLOR;
    float[] hsbvals;
    int r = color.getRed();
    int g = color.getGreen();
    int b = color.getBlue();
    hsbvals = Color.RGBtoHSB(r, g, b, null);
    for (int i=0; i <= rand.nextInt(10); i++) {
      hsbvals[0] += COLOR_STEP;
    }
    if (hsbvals[0] > 1) {
      hsbvals[0] -= 1;
    }
    return Color.getHSBColor(hsbvals[0], hsbvals[1], hsbvals[2]);
  }

  void incFrontAngle() {
    frontAngle += ANGLE_STEP;
    if (frontAngle >= 360) { frontAngle -= 360; }
  }

  void decFrontAngle() {
    frontAngle -= ANGLE_STEP;
    if (frontAngle < 0) { frontAngle += 360; }
  }

  void incRearAngle() {
    rearAngle += ANGLE_STEP;
    if (rearAngle >= 360) { rearAngle -= 360; }
  }

  void decRearAngle() {
    rearAngle -= ANGLE_STEP;
    if (rearAngle < 0) { rearAngle += 360; }
  }

  void incFrontElongation() {
    frontElongation += ELONGATION_STEP;
    if (frontElongation > MAX_ELONGATION) { frontElongation = MAX_ELONGATION; }
  }

  void decFrontElongation() {
    frontElongation -= ELONGATION_STEP;
    if (frontElongation < MIN_ELONGATION) { frontElongation = MIN_ELONGATION; }
  }

  void incRearElongation() {
    rearElongation += ELONGATION_STEP;
    if (rearElongation > MAX_ELONGATION) { rearElongation = MAX_ELONGATION; }
  }

  void decRearElongation() {
    rearElongation -= ELONGATION_STEP;
    if (rearElongation < MIN_ELONGATION) { rearElongation = MIN_ELONGATION; }
  }

  void incExpandX() {
    expandX += EXPAND_STEP;
    if (expandX > MAX_EXPAND) { expandX = MAX_EXPAND; }
  }

  void decExpandX() {
    expandX -= EXPAND_STEP;
    if (expandX < MIN_EXPAND) { expandX = MIN_EXPAND; }
  }

  void incExpandY() {
    expandY += EXPAND_STEP;
    if (expandY > MAX_EXPAND) { expandY = MAX_EXPAND; }
  }

  void decExpandY() {
    expandY -= EXPAND_STEP;
    if (expandY < MIN_EXPAND) { expandY = MIN_EXPAND; }
  }

  void incIteration() {
    iterations += 1;
    if (iterations > MAX_ITERATIONS) { iterations = MAX_ITERATIONS; }
  }

  void decIteration() {
    iterations -= 1;
    if (iterations < 0) { iterations = 0; }
  }

  void incGradient() {
    gradient += GRADIENT_STEP;
    if (gradient > MAX_GRADIENT) { gradient = MAX_GRADIENT; }
  }

  void decGradient() {
    gradient -= GRADIENT_STEP;
    if (gradient <= 0) { gradient += GRADIENT_STEP; }
  }

  void incColor() {
    float[] hsbvals;
    int r = color.getRed();
    int g = color.getGreen();
    int b = color.getBlue();
    hsbvals = Color.RGBtoHSB(r, g, b, null);
    hsbvals[0] += COLOR_STEP;
    if (hsbvals[0] > 1) { hsbvals[0] -= 1; }
    color = Color.getHSBColor(hsbvals[0], hsbvals[1], hsbvals[2]);
  }

  void decColor() {
    float[] hsbvals;
    int r = color.getRed();
    int g = color.getGreen();
    int b = color.getBlue();
    hsbvals = Color.RGBtoHSB(r, g, b, null);
    hsbvals[0] -= COLOR_STEP;
    if (hsbvals[0] < 0) { hsbvals[0] += 1; }
    color = Color.getHSBColor(hsbvals[0], hsbvals[1], hsbvals[2]);
  }

  void changeShape() {
    if (shape == "line") { shape = "rect"; }
    else if (shape == "rect") { shape = "oval"; }
    else if (shape == "oval") { shape = "line"; }
  }
}

class GenerationCanvas extends Canvas implements MouseListener {
  final double MAX_SCALE = 2.0;
  final double MIN_SCALE = 0.1;
  final double SCALE_STEP = 0.1;
  boolean showValues;
  double scale;
  int width, height;
  Graphics buffer;
  Image bufferImage;
  int generations;
  Biomorph parent;
  ArrayList<Biomorph> children;

  GenerationCanvas() {
    showValues = false;
    children = new ArrayList<Biomorph>();
    restart(Biomorph.getDefault());
    addMouseListener(this);
  }

  void restart(Biomorph initBiomorph) {
    scale = 1.0;
    generations = 0;
    createNewGeneration(initBiomorph);
  }

  void incScale() {
    scale += SCALE_STEP;
    if (scale > MAX_SCALE) { scale = MAX_SCALE; }
  }

  void decScale() {
    scale -= SCALE_STEP;
    if (scale < MIN_SCALE) { scale = MIN_SCALE; }
  }

  void createNewGeneration(Biomorph newParent) {
    Biomorph child;

    generations++;
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
    child.incExpandX();
    children.add(child);

    child = parent.clone();
    child.decExpandX();
    children.add(child);

    child = parent.clone();
    child.incExpandY();
    children.add(child);

    child = parent.clone();
    child.decExpandY();
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

    child = parent.clone();
    child.incColor();
    children.add(child);

    child = parent.clone();
    child.decColor();
    children.add(child);

    child = parent.clone();
    child.changeShape();
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
    final int DY = height/4/2+10; // przesuniecie biomorfa w dol
    final int INIT_ANGLE = 0; // 0 - biomorfy skierowane w gore
    final int BM_W = width/5; // szerokosc biomorfa
    final int BM_H = height/4; // wysokosc biomorfa
    Iterator<Biomorph> childIter = children.iterator();
    Biomorph child;

    buffer.clearRect(0, 0, width, height);
    buffer.setColor(new Color(216, 216, 216));
    buffer.fillRect(0, 0, BM_W, BM_H);
    drawBiomorph(buffer, new Coords(BM_W*0+BM_W/2, BM_H*0+DY),
	parent, INIT_ANGLE, parent.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*1+BM_W/2, BM_H*0+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*2+BM_W/2, BM_H*0+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*3+BM_W/2, BM_H*0+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*4+BM_W/2, BM_H*0+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*0+BM_W/2, BM_H*1+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*1+BM_W/2, BM_H*1+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*2+BM_W/2, BM_H*1+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*3+BM_W/2, BM_H*1+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*4+BM_W/2, BM_H*1+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*0+BM_W/2, BM_H*2+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*1+BM_W/2, BM_H*2+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*2+BM_W/2, BM_H*2+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*3+BM_W/2, BM_H*2+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*4+BM_W/2, BM_H*2+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*0+BM_W/2, BM_H*3+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*1+BM_W/2, BM_H*3+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*2+BM_W/2, BM_H*3+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*3+BM_W/2, BM_H*3+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    drawBiomorph(buffer, new Coords(BM_W*4+BM_W/2, BM_H*3+DY),
	child=childIter.next(), INIT_ANGLE, child.frontElongation);
    buffer.setColor(Color.BLACK);

    if (showValues) {
      buffer.drawString(parent.frontAngle+"", BM_W*1+10, BM_H*0+18);
      buffer.drawString(parent.rearAngle+"", BM_W*3+10, BM_H*0+18);
      buffer.drawString(parent.frontElongation+"", BM_W*0+10, BM_H*1+18);
      buffer.drawString(parent.rearElongation+"", BM_W*2+10, BM_H*1+18);
      buffer.drawString(String.format("%.1f", parent.expandX), BM_W*4+10,
	  BM_H*1+18);
      buffer.drawString(String.format("%.1f", parent.expandY), BM_W*1+10,
	  BM_H*2+18);
      buffer.drawString(parent.iterations+"", BM_W*3+10, BM_H*2+18);
      buffer.drawString(String.format("%.1f", parent.gradient), BM_W*0+10,
	  BM_H*3+18);
      buffer.drawString(String.format("%.1f", getHue(parent.color)), BM_W*2+10,
	  BM_H*3+18);
    }
    buffer.drawString("Pokolenie " + generations, width/2-32, height-16);
  }
 
  double getHue(Color c) {
    float[] hsbv = Color.RGBtoHSB(c.getRed(), c.getGreen(), c.getBlue(), null);
    return hsbv[0];
  }

  void drawBiomorph(Graphics g, Coords cursor, Biomorph bm, int angle,
      int elongation) {
    if (angle >= 360) { // zakres kata od 0 do 359 stopni
      angle -= 360;
    } else if (angle < 0) {
      angle += 360;
    }
    cursor = drawShapeAngle(g, cursor, angle, elongation,
       	bm.expandX, bm.expandY, scale, bm.color, bm.shape);
    if (bm.iterations > 0) {
      if (angle == 0) { // kat zero stopni to kierunek do gory
	drawBiomorph(g, cursor, new Biomorph(bm), 0 + bm.frontAngle,
	    bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 0 - bm.frontAngle,
	    bm.frontElongation);
      } else if (angle < 90) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0, bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 90, bm.frontElongation);
      } else if (angle == 90) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0 + bm.frontAngle,
	    bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 - bm.rearAngle,
	    bm.rearElongation);
      } else if (angle < 180) {
	drawBiomorph(g, cursor, new Biomorph(bm), 90, bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 180, bm.rearElongation);
      } else if (angle == 180) {
	drawBiomorph(g, cursor, new Biomorph(bm), 180 + bm.rearAngle,
	    bm.rearElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 - bm.rearAngle,
	    bm.rearElongation);
      } else if (angle < 270) {
	drawBiomorph(g, cursor, new Biomorph(bm), 180, bm.rearElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 270, bm.frontElongation);
      } else if (angle == 270) {
	drawBiomorph(g, cursor, new Biomorph(bm), 0 - bm.frontAngle,
	    bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 180 + bm.rearAngle,
	    bm.rearElongation);
      } else if (angle < 360) {
	drawBiomorph(g, cursor, new Biomorph(bm), 270, bm.frontElongation);
	drawBiomorph(g, cursor, new Biomorph(bm), 0, bm.frontElongation);
      }
    }
  }

  /**
   * Rysuje dana figure (line, rect lub oval) pod danym katem, rozciagnieta
   * w poziomie i pione o dane wspolczynniki, ogolnie powiekszona przez kolejny
   * wspolczynnik.
   */
  Coords drawShapeAngle(Graphics g, Coords start, int angle, int len,
      double expandX, double expandY, double scale, Color color,
      String shape) {
    Coords dest;
    double angleD, lenD;
    double diffXD, diffYD;

    angleD = angle + 90; // przyjmujemy, ze 0 stopni to kierunek do gory
    angleD = Math.toRadians(angleD);
    lenD = len;
    // rozciaganie
    diffXD = (lenD * Math.cos(angleD)) * expandX * scale;
    diffYD = (lenD * Math.sin(angleD) * (-1)) * expandY * scale;
    dest = Coords.add(start, new Coords((int) diffXD, (int) diffYD));
    g.setColor(color);
    if (shape == "line") {
      g.drawLine(start.x, start.y, dest.x, dest.y);
    } else {
      int x = Math.min(start.x, dest.x);
      int y = Math.min(start.y, dest.y);
      int w = Math.max(start.x, dest.x) - Math.min(start.x, dest.x);
      int h = Math.max(start.y, dest.y) - Math.min(start.y, dest.y);
      if (shape == "rect") {
	if (w == 0) { x--; w = 2; }
	if (h == 0) { y--; h = 2; }
	g.fillRect(x, y, w, h);
      } else if (shape == "oval") {
	if (w != 0 && h != 0) {
	  x -= 1; y -= 1;
	  w += 2; h += 2;
	  g.fillOval(x, y, w, h);
	}
      }
    }

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
        createNewGeneration(children.get(12));
      } else if (x < width/5*5) {
        createNewGeneration(children.get(13));
      }
    } else if (y < height/4*4) {
      if (x < width/5*1) {
        createNewGeneration(children.get(14));
      } else if (x < width/5*2) {
        createNewGeneration(children.get(15));
      } else if (x < width/5*3) {
        createNewGeneration(children.get(16));
      } else if (x < width/5*4) {
        createNewGeneration(children.get(17));
      } else if (x < width/5*5) {
        createNewGeneration(children.get(18));
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

class Controls extends Panel implements ActionListener, ItemListener {
  GenerationCanvas canvas;

  public Controls(GenerationCanvas canvas) {
    Button b = null;
    Checkbox c = null;

    this.canvas = canvas;
    add(new Label("                    "));
    b = new Button("Zrestartuj");
    b.addActionListener(this);
    add(b);
    b = new Button("Wybierz losowo");
    b.addActionListener(this);
    add(b);
    b = new Button("Przetasuj");
    b.addActionListener(this);
    add(b);
    b = new Button("Powiêksz");
    b.addActionListener(this);
    add(b);
    b = new Button("Pomniejsz");
    b.addActionListener(this);
    add(b);
    add(new Label(" "));
    c = new Checkbox("Poka¿ wartoœci genów", false);
    c.addItemListener(this);
    add(c);
  }

  public void actionPerformed(ActionEvent e) {
    String label = e.getActionCommand();
    if (label.equals("Zrestartuj")) {
      canvas.restart(Biomorph.getDefault());
    }
    if (label.equals("Wybierz losowo")) {
      canvas.createNewGeneration(canvas.children.get(
          Biomorphs.rand.nextInt(canvas.children.size())));
    }
    if (label.equals("Przetasuj")) {
      canvas.restart(Biomorph.getRandom());
    }
    if (label.equals("Powiêksz")) {
      canvas.incScale();
    }
    if (label.equals("Pomniejsz")) {
      canvas.decScale();
    }
    canvas.paintBuffer();
  }

  public void itemStateChanged(ItemEvent e) {
    canvas.showValues = !canvas.showValues;
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