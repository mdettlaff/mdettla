/**
 * @author Micha³ Dettlaff
 * @version 1.7
 */

/*
<applet width=700 height=380
	code="TypingTest.class"
	archive="TypingTest.jar">
  <param name=txtSourceDir value=txt>
</applet>
*/

import java.applet.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.jar.*;
import java.util.ArrayList;
import java.util.Date;
import java.util.Random;

/**
 * Test szybkosci pisania.
 */
public class TypingTest extends Applet
    implements KeyListener, MouseListener {

  static final int FONT_SIZE=16;
  final int X_MAR=25;
  final int Y_MAR=25;
  final int LINE_HEIGHT=18;
  final int MAX_LINES=9; // ile pelnych wierszy sie zmiesci na ekranie
  Graphics buffer; // grafike bedziemy rysowac w buforze, zeby nie migalo
  Image bufferImage; // pojedyncza klatka, do ktorej wrzucamy gotowy bufor
  static String txtDir; // katalog z plikami do przepisywania
  static int maxCharsInLine;
  File currentTxtFile;
  int width, height; // rozmiary apletu
  boolean showPolishChars;
  boolean showResults;
  boolean isTimerOn;
  boolean showWelcome;
  long timeStarted;
  long timeFinished;
  int charsTyped;
  int mistakesCount;
  Text text;
  Text typedText;
  Text mistakes;
  Font font;
  Font fontBig;
  Font fontSmall;

  public void init() {
    showPolishChars=true;
    isTimerOn=false;
    showResults=false;
    showWelcome=true;
    charsTyped=0;
    mistakesCount=0;
    maxCharsInLine=65;
    txtDir = getParameter("txtSourceDir");
    loadNewTextToType(txtDir);

    width=getWidth();
    height=getHeight();
    setBackground(Color.WHITE);
    setForeground(Color.BLACK);
    font = new Font("Monospaced", Font.PLAIN, FONT_SIZE);
    fontBig = new Font("Serif", Font.PLAIN, 18);
    fontSmall = new Font("Serif", Font.PLAIN, 16);
    bufferImage = createImage(width, height);
    buffer = bufferImage.getGraphics();
    paintBuffer();

    addKeyListener(this);
    addMouseListener(this);
  }

  public void destroy() {
    removeKeyListener(this);
    removeMouseListener(this);
  }

  public void paint(Graphics g) {
    update(g);
  }

  void paintBuffer() {
    drawInBuffer();
    repaint();
  }

  public void update(Graphics g) {
    g.drawImage(bufferImage, 0, 0, this);
    getToolkit().sync(); // ponoc polepsza wyglad w niektorych systemach
  }

  void drawInBuffer() {
    buffer.clearRect(0, 0, width, height);

    if (showWelcome) {
      buffer.setColor(Color.BLACK);
      buffer.setFont(fontBig);
      buffer.drawString("Test prêdkoœci pisania", width/2-90, height/2-100);
      buffer.setFont(fontSmall);
      buffer.drawString("Po przepisaniu ka¿dej linii naciœnij ENTER",
		           width/2-137, height/2-40);
      buffer.drawString("Czas zacznie byæ mierzony, kiedy zaczniesz pisaæ",
		           width/2-158, height/2-15);
      buffer.drawString("F2 wy³¹cza/w³¹cza polskie znaki",
		           width/2-108, height/2+10);
      buffer.setFont(fontBig);
      buffer.drawString("Kliknij, aby rozpocz¹æ", width/2-90, height/2+50);
      buffer.setColor(Color.BLUE);
      buffer.drawString("autor: Micha³ Dettlaff", width/2-88, height/2+112);
      buffer.setColor(Color.BLACK);

    } else {

      buffer.setFont(font);
      int scrollLines=0; // o ile przesunac tekst w gore
      if (typedText.size() > MAX_LINES) {
	scrollLines=typedText.size() - MAX_LINES;
      }

      if (showResults) {
	long typingTime = timeFinished - timeStarted; // w milisekundach
	double tt = ((double) typingTime) / 1000;
	double ct = charsTyped;
	double speed = 60 * ct / tt;
	long typingTimeS = typingTime / 1000; // w sekundach
	int minutes = (int) typingTimeS / 60;
	int seconds = (int) typingTimeS % 60;
	if (minutes > 0) {
	buffer.drawString("Wynik: " + String.format("%.1f", speed) +
	  " znaków/min, " + mistakesCount + " b³êdów (" + charsTyped +
	    " znaków w " + minutes + " min " + seconds + " s)",
	      X_MAR, Y_MAR+((typedText.size()-scrollLines)*2-1)*LINE_HEIGHT);
	} else {
	buffer.drawString("Wynik: " + String.format("%.1f", speed) +
	  " znaków/min, " + mistakesCount + " b³êdów (" + charsTyped +
	    " znaków w " + seconds + " sekund)",
	      X_MAR, Y_MAR+((typedText.size()-scrollLines)*2-1)*LINE_HEIGHT);
	}
	buffer.drawString("Naciœnij ENTER, aby rozpocz¹æ kolejny test",
	  X_MAR, Y_MAR+(typedText.size()-scrollLines)*2*LINE_HEIGHT);
      }

      buffer.setColor(Color.BLUE);
      for (int i=scrollLines; i < typedText.size()
				  && i < (scrollLines+MAX_LINES); i++) {
	buffer.drawString(typedText.get(i), X_MAR, Y_MAR+(2*(i-scrollLines)+1)*
								LINE_HEIGHT);
      }
      buffer.setColor(Color.BLACK);

      for (int j=scrollLines; j < text.size()
				  && j < (scrollLines+MAX_LINES+1); j++) {
	buffer.drawString(text.get(j), X_MAR, Y_MAR+2*(j-scrollLines)*
								LINE_HEIGHT);
      }

      buffer.setColor(Color.RED);
      for (int k=scrollLines; k < mistakes.size()
				  && k < (scrollLines+MAX_LINES); k++) {
	buffer.drawString(mistakes.get(k), X_MAR, Y_MAR+(2*(k-scrollLines)+1)*
								LINE_HEIGHT);
      }
      buffer.setColor(Color.BLACK);
    }
    buffer.drawRect(0, 0, width-1, height-1);
  }

  public void mouseClicked(MouseEvent e) {
    showWelcome=false;
    paintBuffer();
    e.consume();
  }

  public void mousePressed(MouseEvent e) {
  }

  public void mouseReleased(MouseEvent e) {
  }

  public void mouseEntered(MouseEvent e) {
  }

  public void mouseExited(MouseEvent e) {
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
    int ch=e.getKeyCode();

    if (ch == KeyEvent.VK_F2) {
      if (!isTimerOn && !showResults) { // widac tekst, ale jeszcze nie piszemy
	if (showPolishChars) {
	  text.removePolishChars();
	} else {
	  text = new Text();
	  read(text, currentTxtFile.toString());
	  text.breakLines(maxCharsInLine);
	}
	paintBuffer();
      }
      showPolishChars=!showPolishChars;
    }
    e.consume();
  }

  public void keyTyped(KeyEvent e) {
    char ch=e.getKeyChar();

    if (!showResults && !showWelcome) {
      if (ch == KeyEvent.VK_BACK_SPACE) {
	if (typedText.getLast().length() > 0) { // inaczej RuntimeException
	  String s = typedText.getLast();
	  s = s.substring(0, s.length()-1);
	  typedText.setLast(s);
	  charsTyped--;
	  // wymazujemy tez z tablicy przechowujacej pomylki
	  s = mistakes.getLast();
	  if (s.charAt(s.length()-1) != ' ') {
	    mistakesCount--;
	  }
	  s = s.substring(0, s.length()-1);
	  mistakes.setLast(s);
	}
      } else if (ch == KeyEvent.VK_ENTER) {
	if (charsTyped > 0) {
	  // sprawdza, czy nie wpisalismy za krotkiej linii, bo to blad
	  // jesli tak, to dodajemy podkreslenie az do konca linii
	  if (typedText.getLast().length() < text.get(
				     typedText.size()-1).length()) {
	    String underline = "";
	    int gapSize=text.get(typedText.size()-1).length()
				- typedText.getLast().length();
	    for (int i=0; i < gapSize; i++) {
	      underline += '_';
	      mistakesCount++;
	    }
	    mistakes.setLast(mistakes.getLast().concat(underline));
	  }
	  mistakes.add(""); // nowa linia do zapamietywania bledow
	  typedText.add("");
	  charsTyped++; // ENTER to tez znak
	  if (typedText.size() > text.size()) { // jesli to byla ostatnia linia
	    charsTyped--; // ostatniego entera nie liczymy
	    timeFinished=(new Date()).getTime();
	    showResults=true;
	    isTimerOn=false;
	  }
	}
      } else {
	if (!isTimerOn) {
	  timeStarted=(new Date()).getTime();
	  isTimerOn=true;
	}
	String s = typedText.getLast();
        s += ch;
	typedText.setLast(s);
	charsTyped++;
	// jesli jest blad, zapisujemy go w tablicy mistakes
	s = text.get(typedText.size()-1);
	if (typedText.getLast().length() > s.length() // przekroczona linia
	    || ch != s.charAt(typedText.getLast().length()-1)) {
	  s = mistakes.getLast();
	  s += '_';
	  mistakes.setLast(s);
	  mistakesCount++;
	} else {
	  s = mistakes.getLast();
	  s += ' ';
	  mistakes.setLast(s);
	}
      }
      paintBuffer();
    } else if (!showWelcome) {
      if (ch == KeyEvent.VK_ENTER) { // restart, nowy tekst
	showResults=false;
	charsTyped=0;
	loadNewTextToType(txtDir);
        paintBuffer();
      }
    } else {
      showWelcome=false;
      paintBuffer();
    }
    e.consume();
  }

  /** Wczytuje nowy losowo wybrany plik tekstowy z danego katalogu. */
  void loadNewTextToType(String directory) {
    File[] files = new File(directory).listFiles();
    Random rand = new Random(new Date().getTime());

    text = new Text();
    currentTxtFile = files[rand.nextInt(files.length)];
    read(text, currentTxtFile.toString());
    text.breakLines(maxCharsInLine);
    if (!showPolishChars) {
      text.removePolishChars();
    }

    typedText = new Text();
    typedText.add("");
    mistakesCount=0;
    mistakes = new Text();
    mistakes.add("");
  }

  /** Wczytuje kolejne linie z pliku file do tablicy lines */
  public void read(Text lines, String file) {
    try {
      InputStream is = getClass().getResourceAsStream(file);
      BufferedReader in = new BufferedReader(new InputStreamReader(is));
      try {
	String s;
	while((s = in.readLine()) != null) {
	  lines.add(s);
	}
      } finally {
	in.close();
      }
    } catch(IOException e) {
      throw new RuntimeException(e);
    }
  }

  public String getAppletInfo() {
    return "Test prêdkoœci pisania. U¿ywa plików tekstowych z katalogu okreœlonego parametrem.";
  }
}

/**
 * Tekst w postaci kolejnych linii umieszczonych w tablicy ArrayList
 */
class Text extends ArrayList<String> {

  public void removePolishChars() {
    for (int i=0; i < size(); i++) {
      set(i, get(i).replace('¹', 'a'));
      set(i, get(i).replace('æ', 'c'));
      set(i, get(i).replace('ê', 'e'));
      set(i, get(i).replace('³', 'l'));
      set(i, get(i).replace('ñ', 'n'));
      set(i, get(i).replace('ó', 'o'));
      set(i, get(i).replace('œ', 's'));
      set(i, get(i).replace('¿', 'z'));
      set(i, get(i).replace('Ÿ', 'z'));

      set(i, get(i).replace('¥', 'A'));
      set(i, get(i).replace('Æ', 'C'));
      set(i, get(i).replace('Ê', 'E'));
      set(i, get(i).replace('£', 'L'));
      set(i, get(i).replace('Ñ', 'N'));
      set(i, get(i).replace('Ó', 'O'));
      set(i, get(i).replace('Œ', 'S'));
      set(i, get(i).replace('¯', 'Z'));
      set(i, get(i).replace('', 'Z'));
    }
  }

  public void breakLines(int lineLength) {
    int eolIndex=0;
    for (int i=0; i < size(); i++) {
      for (int j=1; j < get(i).length() && j <= lineLength; j++) {
	if (get(i).charAt(j) == ' ' || get(i).charAt(j) == '\t') {
	  eolIndex=j;
	}
	if (j == lineLength) { // lamiemy linie
	  add(i+1, get(i).substring(eolIndex+1));
	  set(i, get(i).substring(0, eolIndex));
	}
      }
    }
  }

  /** Dlaczego takiej metody nie ma w bibliotece standardowej? */
  public String getLast() {
    return get(size()-1);
  }

  public void setLast(String s) {
    set(size()-1, s);
  }
}

