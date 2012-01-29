package mdettla.regexp;

class Parser {

	private CharReader regExp;

	public Expression parse(String regExp) {
		this.regExp = new CharReader(regExp);
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
		while (Character.valueOf('a').equals(regExp.getCurrent())
				|| Character.valueOf('(').equals(regExp.getCurrent())) {
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

	private boolean accept(Character c) {
		if (c.equals(regExp.getCurrent())) {
			regExp.next();
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
