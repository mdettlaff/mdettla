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

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		Star other = (Star) obj;
		return expression.equals(other.expression);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((expression == null) ? 0 : expression.hashCode());
		return result;
	}
}
