package mdettla.regexp;

class Alternative implements Expression {

	private final Expression left;
	private final Expression right;

	public Alternative(Expression left, Expression right) {
		this.left = left;
		this.right = right;
	}

	@Override
	public boolean match(CharReader chars) {
		return left.match(chars) || right.match(chars);
	}
}
