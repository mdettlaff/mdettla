package mdettla.regexp;

class Sequence implements Expression {

	private final Expression left;
	private final Expression right;

	public Sequence(Expression left, Expression right) {
		this.left = left;
		this.right = right;
	}

	@Override
	public boolean match(CharReader chars) {
		return left.match(chars) && right.match(chars);
	}
}
