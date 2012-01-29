package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;
import static mdettla.regexp.ExpressionAssertions.assertNoMatch;

import org.junit.Test;

public class PlusTest {

	@Test
	public void testSymbolPlus() {
		Plus plus = new Plus(new Symbol('a'));
		assertMatch(plus, "a", 1);
		assertMatch(plus, "a", 1);
		assertMatch(plus, "aa", 2);
		assertMatch(plus, "aaa", 3);
		assertNoMatch(plus, "b", 0);
		assertNoMatch(plus, "ba", 0);
		assertNoMatch(plus, "", 0);
	}

	@Test
	public void testSequencePlus() {
		Plus plus = new Plus(new Sequence(new Symbol('a'), new Symbol('b')));
		assertMatch(plus, "ab", 2);
		assertMatch(plus, "abab", 4);
		assertMatch(plus, "ababab", 6);
		assertNoMatch(plus, "aa", 1);
		assertNoMatch(plus, "cab", 0);
		assertNoMatch(plus, "a", 1);
		assertNoMatch(plus, "b", 0);
		assertNoMatch(plus, "", 0);
	}
}
