package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;
import static mdettla.regexp.ExpressionAssertions.assertNoMatch;

import org.junit.Test;

public class SymbolTest {

	@Test
	public void test() {
		Symbol symbol = new Symbol('a');
		assertMatch(symbol, "a", 1);
		assertNoMatch(symbol, "b", 0);
		assertNoMatch(symbol, "", 0);
	}
}
