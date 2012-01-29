package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;

import org.junit.Test;

public class StarTest {

	@Test
	public void testSymbolStar() {
		Star star = new Star(new Symbol('a'));
		assertMatch(star, "", 0);
		assertMatch(star, "a", 1);
		assertMatch(star, "aa", 2);
		assertMatch(star, "aaa", 3);
		assertMatch(star, "b", 0);
		assertMatch(star, "ba", 0);
	}

	@Test
	public void testSequenceStar() {
		Star star = new Star(new Sequence(new Symbol('a'), new Symbol('b')));
		assertMatch(star, "", 0);
		assertMatch(star, "ab", 2);
		assertMatch(star, "abab", 4);
		assertMatch(star, "ababab", 6);
		assertMatch(star, "aa", 1);
		assertMatch(star, "cab", 0);
		assertMatch(star, "a", 1);
		assertMatch(star, "b", 0);
	}
}
