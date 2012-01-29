package mdettla.regexp;

public class RegExp {

	private final Expression expression;

	public RegExp(String regExp) {
		Parser parser = new Parser();
		expression = parser.parse(regExp);
	}

	public boolean match(String string) {
		return expression.match(new CharIterator(string));
	}
}
