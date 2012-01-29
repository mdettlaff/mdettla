package mdettla.regexp;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class RegExpTest {

	@Test
	public void test() {
		assertTrue(new RegExp("a").match("aa"));
		assertFalse(new RegExp("a").match("b"));
	}
}
