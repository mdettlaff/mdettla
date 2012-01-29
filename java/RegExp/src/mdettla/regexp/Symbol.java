package mdettla.regexp;

class Symbol implements Expression {

	private final Character symbol;

	public Symbol(char symbol) {
		this.symbol = symbol;
	}

	@Override
	public boolean match(CharIterator chars) {
		if (!chars.isCompleted() && symbol.equals(chars.peek())) {
			chars.next();
			return true;
		}
		return false;
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null || getClass() != obj.getClass()) {
			return false;
		}
		Symbol other = (Symbol) obj;
		return symbol.equals(other.symbol);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((symbol == null) ? 0 : symbol.hashCode());
		return result;
	}

	@Override
	public String toString() {
		return String.valueOf(symbol);
	}
}
