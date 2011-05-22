package mdettla.fuzzy;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class FuzzySetTest {

	@Test
	public void testTriangularFuzzySet() {
		FuzzySet fs = new FuzzySet(0, 1, 2);
		assertEquals(0, fs.membership(-1));
		assertEquals(0, fs.membership(0));
		assertEquals(0.25, fs.membership(0.25));
		assertEquals(0.5, fs.membership(0.5));
		assertEquals(1, fs.membership(1));
		assertEquals(0.5, fs.membership(1.5));
		assertEquals(0, fs.membership(2));
		assertEquals(0, fs.membership(3));
	}

	@Test
	public void testLeftSideOpenFuzzySet() {
		FuzzySet fs = new FuzzySet(Double.NEGATIVE_INFINITY, 1, 2);
		assertEquals(1, fs.membership(Double.NEGATIVE_INFINITY));
		assertEquals(1, fs.membership(-1));
		assertEquals(1, fs.membership(1));
		assertEquals(0.5, fs.membership(1.5));
		assertEquals(0, fs.membership(2));
		assertEquals(0, fs.membership(3));
	}

	@Test
	public void testRightSideOpenFuzzySet() {
		FuzzySet fs = new FuzzySet(0, 1, Double.POSITIVE_INFINITY);
		assertEquals(0, fs.membership(-1));
		assertEquals(0, fs.membership(0));
		assertEquals(0.5, fs.membership(0.5));
		assertEquals(1, fs.membership(1));
		assertEquals(1, fs.membership(Double.POSITIVE_INFINITY));
	}
}
