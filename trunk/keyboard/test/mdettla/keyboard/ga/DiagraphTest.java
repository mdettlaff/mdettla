package mdettla.keyboard.ga;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

import org.junit.Test;

public class DiagraphTest {

	@Test
	public void testEquals() {
		assertEquals(new Diagraph('a', 'b'), new Diagraph('a', 'b'));
		assertFalse(new Diagraph('a', 'b').equals(new Diagraph('c', 'd')));
	}
}
