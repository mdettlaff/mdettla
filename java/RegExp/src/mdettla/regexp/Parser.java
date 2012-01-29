package mdettla.regexp;

class Parser {

	private CharReader regExp;

	public Expression parse(String regExp) {
		this.regExp = new CharReader(regExp);
		Expression expression = expression();
		checkIfParsingComplete();
		return expression;
	}

	private void checkIfParsingComplete() {
		if (this.regExp.getCurrent() != null) {
			throw new ParseException("superfluous characters at the end of expression");
		}
	}

	private Expression expression() {
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
		Expression left = repetition();
		while (regExp.getCurrent() != null && !isSpecial(regExp.getCurrent())) {
			Expression right = repetition();
			left = new Sequence(left, right);
		}
		return left;
	}

	private Expression repetition() {
		Expression expression = atom();
		if (accept('*')) {
			expression = new Star(expression);
		} else if (accept('+')) {
			expression = new Plus(expression);
		}
		return expression;
	}

	private Expression atom() {
		if (accept('(')) {
			Expression inParens = expression();
			expect(')');
			return inParens;
		}
		Character current = regExp.getCurrent();
		if (accept(current)) {
			return symbol(current);
		}
		return expression();
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
			throw new ParseException("missing '" + c + "'");
		}
	}

	private boolean isSpecial(char c) {
		return "|)*+".indexOf(c) != -1;
	}
}
