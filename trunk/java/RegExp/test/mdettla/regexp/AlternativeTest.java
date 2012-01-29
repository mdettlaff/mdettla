package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;
import static mdettla.regexp.ExpressionAssertions.assertNoMatch;

import org.junit.Test;

public class AlternativeTest {

	@Test
	public void test() {
		Alternative alternative = new Alternative(new Symbol('a'), new Symbol('b'));
		assertMatch(alternative, "a", 1);
		assertMatch(alternative, "b", 1);
		assertNoMatch(alternative, "c", 0);
		assertNoMatch(alternative, "", 0);
	}
}
