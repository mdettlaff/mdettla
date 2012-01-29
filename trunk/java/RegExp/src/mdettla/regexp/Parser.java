package mdettla.regexp;

class Parser {

	private StringBuilder regExp;
	private Character next;

	public Expression parse(String regExp) {
		this.regExp = new StringBuilder(regExp);
		read();
		return regExp();
	}

	private Expression regExp() {
		return alternative();
	}

	private Expression alternative() {
		Expression left = sequence();
		while (accept('|')) {
			Expression right = sequence();
			left = new Alternative(left, right);
		}
		return left;
	}

	private Expression sequence() {
		Expression left = atom();
		while (Character.valueOf('a').equals(next) || Character.valueOf('(').equals(next)) {
			Expression right = atom();
			left = new Sequence(left, right);
		}
		return left;
	}

	private Expression atom() {
		if (accept('(')) {
			Expression inParens = regExp();
			expect(')');
			return inParens;
		} else if (accept('a')) {
			return symbol();
		}
		return regExp();
	}

	private Symbol symbol() {
		return new Symbol('a');
	}

	private void read() {
		if (regExp.length() == 0) {
			next = null;
		} else {
			next = regExp.charAt(0);
			regExp.deleteCharAt(0);
		}
	}

	private boolean accept(Character c) {
		if (c.equals(next)) {
			read();
			return true;
		}
		return false;
	}

	private void expect(char c) {
		if (!accept(c)) {
			throw new ParseException();
		}
	}
}
