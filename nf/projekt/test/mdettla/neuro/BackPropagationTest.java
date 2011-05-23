package mdettla.neuro;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import mdettla.util.Function;

import org.junit.Test;

public class BackPropagationTest {

	@Test
	public void test() {
		Function xor = new Function() {
			@Override public double evaluate(double[] x) {
				boolean p = x[0] > 0;
				boolean q = x[1] > 0;
				boolean result = p != q;
				return result ? 1 : -1;
			}
		};
		double[][] xs = {{-1, -1}, {-1, 1}, {1, -1}, {1, 1}};
		BackPropagation backPropagation = new BackPropagation(xs, xor);
		for (double[] x : xs) {
			double output = backPropagation.getOutput(x);
			assertEquals(xor.evaluate(x), output, 0.2);
		}
		double error = backPropagation.getError(xs);
		assertTrue(error < 0.05);
	}
}
