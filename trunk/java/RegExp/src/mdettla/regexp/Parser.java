package mdettla.regexp;

class Parser {

	private CharReader regExp;

	public Expression parse(String regExp) {
		this.regExp = new CharReader(regExp);
		return regExp();
	}

	// http://www.users.pjwstk.edu.pl/~jms/qnx/help/watcom/wd/regexp.html#RegularExpressionBNF
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
		while (isRegularChar(regExp.getCurrent())) {
			Expression right = atom();
			left = new Sequence(left, right);
		}
		return left;
	}

	private boolean isRegularChar(Character c) {
		return c != null && !c.equals('|') && !c.equals(')');
	}

	private Expression atom() {
		if (accept('(')) {
			Expression inParens = regExp();
			expect(')');
			return inParens;
		}
		Character current = regExp.getCurrent();
		if (accept(current)) {
			return symbol(current);
		}
		return regExp();
	}

	private Symbol symbol(Character c) {
		return new Symbol(c);
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
