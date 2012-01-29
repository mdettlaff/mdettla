package mdettla.regexp;

class CharIterator {

	private final String string;
	private int position;

	public CharIterator(String string) {
		this.string = string;
	}

	public char peek() {
		return string.charAt(position);
	}

	public void next() {
		position++;
	}

	public boolean isCompleted() {
		return position >= string.length();
	}

	int getPosition() {
		return position;
	}
}
