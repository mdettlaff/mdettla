package mdettla.fuzzy;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import mdettla.util.Range;

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

	@Test
	public void testCoverRangeWithFuzzySets() {
		List<FuzzySet> actual = FuzzySet.coverRangeWithFuzzySets(new Range(0, 1), 5);
		List<FuzzySet> expected = new ArrayList<FuzzySet>();
		expected.add(new FuzzySet(Double.NEGATIVE_INFINITY, 0, 0.25));
		expected.add(new FuzzySet(0, 0.25, 0.5));
		expected.add(new FuzzySet(0.25, 0.5, 0.75));
		expected.add(new FuzzySet(0.5, 0.75, 1));
		expected.add(new FuzzySet(0.75, 1, Double.POSITIVE_INFINITY));
		assertEquals(expected, actual);
	}
}
