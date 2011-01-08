package mdettla.imgproc.algorithms.segmentation;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

import org.junit.Test;

public class NCutSegmentationTest {

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
