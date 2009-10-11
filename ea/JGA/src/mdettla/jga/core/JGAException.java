package mdettla.jga.core;

public class JGAException extends Exception {
	private static final long serialVersionUID = 1L;

	public JGAException() {
		super("Błąd algorytmu genetycznego");
	}

	public JGAException(String message) {
		super(message);
	}

	public JGAException(String message, Throwable cause) {
		super(message, cause);
	}
}
