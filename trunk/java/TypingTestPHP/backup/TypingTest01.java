/**
 * @author Michal Dettlaff
 * @version 1.0
 */

/* <applet width=640 height=400 code="TypingTest.class"></applet> */

import java.awt.*;
import java.awt.event.*;
import java.applet.*;
import java.util.Date;
import java.util.ArrayList;

public class TypingTest extends Applet
    implements /*Runnable, */KeyListener {

  static final int FONT_SIZE=15;
  final int X_MAR=28;
  final int Y_MAR=25;
  final int LINE_HEIGHT=18;
  int charsTyped;
  int width, height; // rozmiary apletu
  ArrayList<String> text;
  ArrayList<String> typedText;
  long timeStarted;
  long timeFinished;
  boolean isTimerOn;
  Font font;
  boolean showResults;
//  Thread t  = null;
//  boolean stopFlag;

  public void init() {
    isTimerOn=false;
    showResults=false;
    charsTyped=0;

    text = new ArrayList<String>();
    typedText = new ArrayList<String>();
    text.add("Pierwszy wiersz");
    text.add("Tekst do przepisywania ktory ma w sobie szescdziesiat piec znakow");
    text.add("Ostatni wiersz");
    typedText.add("");

    width=getWidth();
    height=getHeight();
    setBackground(Color.white);
    setForeground(Color.black);
    font = new Font("Monospaced", Font.PLAIN, FONT_SIZE);

    addKeyListener(this);
  }

//  public void start() {
//    t = new Thread(this);
//    stopFlag = false;
//    t.start();
//  }

//  public void run() {
//    while (true) {
//      if (stopFlag) 
//	break;
//    }
//  }
  
//  public void stop() {
//    stopFlag = true;
//    t = null;
//  }

  public void paint(Graphics g) {
    int i;

    g.setFont(font);
    for (i=0; i < text.size(); i++) {
      g.drawString(text.get(i), X_MAR, Y_MAR+2*i*LINE_HEIGHT);
    }

    for (int j=0; j < typedText.size(); j++) {
      g.drawString(typedText.get(j), X_MAR, Y_MAR+(2*j+1)*LINE_HEIGHT);
    }

    if (showResults) {
      long typingTime = (timeFinished - timeStarted) / 1000;
      g.drawString("Wynik: ? znaków/min ("
	       + charsTyped + " znakow w " + typingTime + " sekund)",
	       X_MAR, Y_MAR+(2*i+1)*LINE_HEIGHT);
      g.drawString("Naciœnij ENTER, aby rozpocz¹æ kolejny test",
	       X_MAR, Y_MAR+2*(i+1)*LINE_HEIGHT);
    }

    g.drawRect(0, 0, width-1, height-1);
  }

  public void keyReleased(KeyEvent e) {
  }

  public void keyPressed(KeyEvent e) {
  }

  public void keyTyped(KeyEvent e) {
    char ch=e.getKeyChar();
    if (!showResults) {
      if (ch == KeyEvent.VK_BACK_SPACE) {
	if (typedText.get(typedText.size()-1).length() > 0) { // inaczej
	  String s = typedText.get(typedText.size()-1);     // RuntimeException
	  s = s.substring(0, s.length()-1);
	  typedText.set(typedText.size()-1, s);
	  charsTyped--;
	}
      } else if (ch == KeyEvent.VK_ENTER) {
	if (charsTyped > 0) {
	  if (typedText.size() < text.size()) {
	    typedText.add("");
	  } else {
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
	String s = typedText.get(typedText.size()-1);
        s += ch;
	typedText.set(typedText.size()-1, s);
	charsTyped++;
      }
      repaint();
    } else {
      if (ch == KeyEvent.VK_ENTER) { // restart
	showResults=false;
	charsTyped=0;
	typedText = new ArrayList<String>();
	typedText.add("");
        repaint();
      }
    }
  }

  public String getAppletInfo() {
    return "Test prêdkoœci pisania";
  }
}
