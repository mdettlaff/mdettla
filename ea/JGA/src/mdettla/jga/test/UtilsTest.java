package mdettla.jga.test;

import static org.junit.Assert.assertTrue;

import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.Utils;

import org.junit.Test;

public class UtilsTest {

	@Test
	public void testRandomSample() {
		List<Integer> population = Arrays.asList(5, 3, 1, 2, 4);
		List<Integer> sample = Utils.randomSample(population, 3);
		assertTrue(sample.size() == 3);
		assertTrue(population.containsAll(sample));
		assertTrue(!sample.get(0).equals(sample.get(1)));
		assertTrue(!sample.get(0).equals(sample.get(2)));
		assertTrue(!sample.get(1).equals(sample.get(2)));
	}
}
