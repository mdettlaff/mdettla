package mdettla.keyboard.ga;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

public class KeyboardLayoutTest {

	private final KeyboardLayout qwerty = new KeyboardLayout(null);

	@Test
	public void testGetPhenotype() {
		final String expected =
			"q w e r t y u i o p\n" +
			"a s d f g h j k l ;\n" +
			"z x c v b n m , . ?";
		assertEquals(expected, qwerty.getPhenotype());
	}

	@Test
	public void testIsLeftRightSide() {
		final String leftSide = "qwertasdfgzxcvb";
		final String rightSide = "yuiophjkl;nm,.?";
		for (int i = 0; i < leftSide.length(); i++) {
			assertTrue(qwerty.isLeftSide(leftSide.charAt(i)));
			assertFalse(qwerty.isLeftSide(rightSide.charAt(i)));
			assertTrue(qwerty.isRightSide(rightSide.charAt(i)));
			assertFalse(qwerty.isRightSide(leftSide.charAt(i)));
		}
	}
}
