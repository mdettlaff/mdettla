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

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		Sequence other = (Sequence) obj;
		return left.equals(other.right) && right.equals(other.right);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((left == null) ? 0 : left.hashCode());
		result = prime * result + ((right == null) ? 0 : right.hashCode());
		return result;
	}
}
