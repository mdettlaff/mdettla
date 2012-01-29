package mdettla.regexp;

class CharReader {

	private final String string;
	private int position;

	public CharReader(String string) {
		this.string = string;
		position = 0;
	}

	public Character getCurrent() {
		return position < string.length() ? string.charAt(position) : null;
	}

	public void next() {
		position++;
	}

	int getPosition() {
		return position;
	}
}
