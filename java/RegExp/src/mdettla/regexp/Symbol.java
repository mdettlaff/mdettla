package mdettla.regexp;

class Symbol implements Expression {

	private final Character symbol;

	public Symbol(char symbol) {
		this.symbol = symbol;
	}

	@Override
	public boolean match(CharReader chars) {
		if (symbol.equals(chars.getCurrent())) {
			chars.next();
			return true;
		}
		return false;
	}
}
