package mdettla.imgproc.algorithms.segmentation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Assert;
import org.junit.Test;

public class NCutSegmentationTest {

	@Test
	public void testCutSimple() {
		double[][] weights = {
				{1, 2, 4},
				{2, 1, 3},
				{4, 3, 1}
		};
		int[] indexesA = {0};
		int[] indexesB = {1, 2};
		double actualAB = NCutSegmentation.cut(indexesA, indexesB, weights);
		double expectedAB = 6;
		assertEquals(expectedAB, actualAB, 0.001);
		double actualBA = NCutSegmentation.cut(indexesB, indexesA, weights);
		double expectedBA = 6;
		assertEquals(expectedBA, actualBA, 0.001);
	}

	@Test
	public void testGetSecondSmallestEigenvalue() {
		double[][] matrix = {
				{2, 1},
				{1, 2}
		};
		double[] eigenvector =
			NCutSegmentation.getEigenvectorWithSecondSmallestEigenvalue(matrix);
		assertArrayEquals(new double[] {-0.707, 0.707}, eigenvector, 0.02);
	}

	@Test
	public void testGetIndexOfSecondSmallest() {
		double[] values = {3.5, 1.1, 2.2, 4.3, 5.0};
		assertEquals(2, NCutSegmentation.getIndexOfSecondSmallest(values));
	}

	@Test
	public void testRange() {
		int[] actuals = NCutSegmentation.range(5);
		Assert.assertArrayEquals(new int[] {0, 1, 2, 3, 4}, actuals);
	}

	public void assertArrayEquals(double[] expected, double[] actual, double precision) {
		if (expected.length != actual.length) {
			fail("lengths do not match");
		}
		for (int i = 0; i < expected.length; i++) {
			if (Math.abs(expected[i] - actual[i]) > precision) {
				fail("expected " + expected[i] + ", but was " + actual[i]);
			}
		}
	}
}
