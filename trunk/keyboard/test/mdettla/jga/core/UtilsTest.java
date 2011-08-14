package mdettla.jga.core;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class UtilsTest {

	@Test
	public void testRandomSample() {
		List<Integer> population = Arrays.asList(5, 3, 1, 2, 4);
		List<Integer> sample = Utils.randomSample(population, 3);
		assertEquals(3, sample.size());
		assertTrue(population.containsAll(sample));
		assertFalse(sample.get(0).equals(sample.get(1)));
		assertFalse(sample.get(0).equals(sample.get(2)));
		assertFalse(sample.get(1).equals(sample.get(2)));
	}

	@Test
	public void testVectorDiff() {
		assertEquals(9, Utils.vectorDiff(Arrays.asList(1, 2), Arrays.asList(1, 5)));
	}

	@Test
	public void testIgnorePolishChars() {
		String textWithPlChars = "zażółć gęślą jaźń ZAŻÓŁĆ GĘŚLĄ JAŹŃ";
		String textWithoutPlChars = "zazolc gesla jazn ZAZOLC GESLA JAZN";
		assertEquals(textWithoutPlChars, Utils.replacePolishChars(textWithPlChars));
	}

	@Test
	@SuppressWarnings("unchecked")
	public void testPartition() {
		assertEquals(
				Arrays.asList(Arrays.asList(1, 2), Arrays.asList(3, 4)),
				Utils.partition(Arrays.asList(1, 2, 3, 4), 2));
		assertEquals(
				Arrays.asList(Arrays.asList(1, 2), Arrays.asList(3, 4, 5)),
				Utils.partition(Arrays.asList(1, 2, 3, 4, 5), 2));
		assertEquals(
				Arrays.asList(Arrays.asList(1, 2, 3), Arrays.asList(4, 5, 6)),
				Utils.partition(Arrays.asList(1, 2, 3, 4, 5, 6), 2));
		assertEquals(
				Arrays.asList(Arrays.asList(1), Arrays.asList(2), Arrays.asList(3)),
				Utils.partition(Arrays.asList(1, 2, 3), 3));
		assertEquals(
				Arrays.asList(Arrays.asList(1, 2), Arrays.asList(3, 4), Arrays.asList(5, 6)),
				Utils.partition(Arrays.asList(1, 2, 3, 4, 5, 6), 3));
		for (int i = 1; i < 100; i++) {
			assertEquals(i, sumSizes(Utils.partition(Utils.range(i), 7)));
		}
	}

	private static <T> int sumSizes(List<List<T>> lists) {
		int sumSizes = 0;
		for (List<T> list : lists) {
			sumSizes += list.size();
		}
		return sumSizes;
	}
}
