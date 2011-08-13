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
}
