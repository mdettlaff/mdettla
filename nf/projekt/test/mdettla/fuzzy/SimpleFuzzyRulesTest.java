package mdettla.fuzzy;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.List;

import org.junit.Test;

public class SimpleFuzzyRulesTest {

	@Test
	public void test() {
		Function testFunction = new Function() {
			@Override public double evaluate(double x) {
				return 0.2 * Math.sin(2 * Math.PI * x + Math.PI / 4) + 0.5;
			}
		};
		double[] xs = {0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1};
		List<FuzzySet> fuzzySets = FuzzySet.coverRangeWithFuzzySets(new Range(0, 1), 5);
		SimpleFuzzyRules fuzzyRules =
			new SimpleFuzzyRules(xs, testFunction, fuzzySets);
		for (double x : xs) {
			assertEquals(testFunction.evaluate(x), fuzzyRules.getOutput(x), 0.075);
		}
		double error = fuzzyRules.getError(xs);
		assertTrue(error < 0.11);
	}
}
