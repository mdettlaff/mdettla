package mdettla.regexp;

class Star implements Expression {

	private final Expression expression;

	public Star(Expression expression) {
		this.expression = expression;
	}

	@Override
	public boolean match(CharReader chars) {
		while (expression.match(chars)) {
		}
		return true;
	}
}
