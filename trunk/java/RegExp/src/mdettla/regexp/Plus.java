package mdettla.regexp;

class Plus implements Expression {

	private final Expression expression;

	public Plus(Expression expression) {
		this.expression = expression;
	}

	@Override
	public boolean match(CharReader chars) {
		if (!expression.match(chars)) {
			return false;
		}
		return new Star(expression).match(chars);
	}
}
