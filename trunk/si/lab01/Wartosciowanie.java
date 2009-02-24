import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.LinkedList;

public class Wartosciowanie {
  public static void main(String[] args) throws Exception {
    MyStack<Character> RPNStack = new MyStack<Character>();
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));

    String inputLine = in.readLine();

    // czytamy kolejne symbole ze standardowego wejscia
    for (int i=0; i < inputLine.length(); i++) {
      Character c = inputLine.charAt(i);
      // jesli jest wartosc, odkladamy na stos
      if (c == 'T' || c == 'F') {
	RPNStack.push(c);
      } else if (c == '~') {
	Character a = RPNStack.pop();
	RPNStack.push(compute(a, c));
      } else if (c == '&' || c == 'V'
	  || c == 'I' || c == 'E') {
	Character a = RPNStack.pop();
	Character b = RPNStack.pop();
	RPNStack.push(compute(b, a, c));
      }
    }

    if (!RPNStack.empty()) {
      System.out.println("Wartosc wyrazenia: " + RPNStack.pop());
    } else {
      System.out.println("Nieprawidlowe wyrazenie");
    }
  }

  /**
   * Oblicza wartosc operacji dwuargumentowej.
   */
  static Character compute(Character a, Character b, Character operator) {
    boolean p = false;
    boolean q = false;
    boolean result = false;
    if (a == 'T') {
      p = true;
    }
    if (b == 'T') {
      q = true;
    }
    switch (operator) {
      case '&' : result = p && q; break;
      case 'V' : result = p || q; break;
      case 'I' : result = !(p == true && q == false); break;
      case 'E' : result = p == q; break;
    }
    if (result) {
      return 'T';
    }
    return 'F';
  }

  /**
   * Oblicza wartosc operacji jednoargumentowej.
   */
  static Character compute(Character a, Character operator) {
    boolean p = false;
    boolean result = false;
    if (a == 'T') {
      p = true;
    }
    switch (operator) {
      case '~' : result = !p; break;
    }
    if (result) {
      return 'T';
    }
    return 'F';
  }
}

/**
 * Stos utworzony przy pomocy listy dowiazaniowej
 * na podstawie Thinking In Java 4th Edition
 */
class MyStack<Character> {
  private LinkedList<Character> storage = new LinkedList<Character>();
  public void push(Character v) { storage.addFirst(v); }
  public Character peek() { return storage.getFirst(); }
  public Character pop() { return storage.removeFirst(); }
  public boolean empty() { return storage.isEmpty(); }
  public String toString() { return storage.toString(); }
}

