package mdettla.regexp;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class RegExpTest {

	@Test
	public void test() {
		assertTrue(new RegExp("a").match("a"));
		assertTrue(new RegExp("ab").match("ab"));
		assertTrue(new RegExp("a").match("aa"));
		assertTrue(new RegExp("ab").match("aba"));
		assertTrue(new RegExp("a|b|c").match("c"));
		assertTrue(new RegExp("(ab|cd|ef)+").match("cd"));
		assertTrue(new RegExp("(ab|cd|ef)+").match("abcdabef"));
		assertTrue(new RegExp("a*").match("aaaaa"));
		assertTrue(new RegExp("ba*c").match("baaac"));
		assertTrue(new RegExp("ba*c").match("bc"));
		assertTrue(new RegExp("ba*c").match("bac"));
		assertFalse(new RegExp("a").match("b"));
		assertFalse(new RegExp("ba").match("aba"));
		assertFalse(new RegExp("a").match(""));
		assertFalse(new RegExp("(ab|cd|ef)+").match("a"));
		assertFalse(new RegExp("ba*c").match("baaaa"));
	}
}
