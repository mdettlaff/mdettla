import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

public class Tautologia {
  public static void main(String[] args) throws Exception {
    BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
    List<Character> symbols = new ArrayList<Character>();
    List<Boolean> evaluation = new ArrayList<Boolean>();

    String inputLine = in.readLine();

    // tworzymy liste symboli wystepujacych w formule
    for (int i=0; i < inputLine.length(); i++) {
      Character c = (Character) inputLine.charAt(i);
      if (Character.isLetter(c) && c != 'V' && c != 'I' && c != 'E') {
	if (!symbols.contains(c)) {
	  symbols.add(c);
	}
      }
    }
    try {
      if (isTautology(inputLine, symbols, evaluation)) {
	System.out.println("jest tautologia");
      } else {
	System.out.println("nie jest tautologia");
      }
    } catch (BadFormulaException bfe) {
      System.out.println("Nieprawidlowe wyrazenie");
    }

    /*try {
      System.out.println("Wartosc wyrazenia: " + isFormulaTrue(inputLine));
    } catch (BadFormulaException bfe) {
      System.out.println("Nieprawidlowe wyrazenie");
    }*/
  }

  public static boolean isTautology(String formula, List<Character> symbols,
      List<Boolean> evaluation) throws BadFormulaException {
    if (evaluation.size() < symbols.size()) {
      List<Boolean> eval1 = new ArrayList<Boolean>(evaluation);
      eval1.add((Boolean) true);
      List<Boolean> eval2 = new ArrayList<Boolean>(evaluation);
      eval2.add((Boolean) false);
      if (!isTautology(formula, symbols, eval1) || 
	  !isTautology(formula, symbols, eval2)) {
	return false;
      }
    } else if (evaluation.size() == symbols.size()) {
      String newFormula = new String(formula);
      for (int i=0; i < symbols.size(); i++) {
	Character c = symbols.get(i);
	String bool = "F";
	if (evaluation.get(i)) {
	  bool = "T";
	}
	newFormula = newFormula.replace("" + c, bool);
      }
      if (!isFormulaTrue(newFormula)) {
	return false;
      }
    }
    return true;
  }

  public static boolean isFormulaTrue(String formula)
    throws BadFormulaException {
    MyStack<Character> RPNStack = new MyStack<Character>();

    // czytamy kolejne symbole ze standardowego wejscia
    for (int i=0; i < formula.length(); i++) {
      Character c = formula.charAt(i);
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
    if (RPNStack.empty()) {
      throw new BadFormulaException();
    }
    return RPNStack.pop() == 'T';
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

class BadFormulaException extends Exception {
}

