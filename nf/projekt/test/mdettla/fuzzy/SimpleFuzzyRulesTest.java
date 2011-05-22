package mdettla.fuzzy;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class SimpleFuzzyRulesTest {

	@Test
	public void test() {
		SimpleFuzzyRules fuzzyRules =
			new SimpleFuzzyRules(getData(), getFuzzySets());
		for (double x = 0; x <= 1; x += 0.1) {
			assertEquals(testFunction(x), fuzzyRules.getOutput(x), 0.075);
		}
	}

	private double testFunction(double x) {
		return 0.2 * Math.sin(2 * Math.PI * x + Math.PI / 4) + 0.5;
	}

	private double[][] getData() {
		return
		new double[][] {
				{0.0, 0.641},
				{0.1, 0.698},
				{0.2, 0.678},
				{0.3, 0.591},
				{0.4, 0.469},
				{0.5, 0.359},
				{0.6, 0.302},
				{0.7, 0.322},
				{0.8, 0.409},
				{0.9, 0.531},
				{1.0, 0.641}
		};
	}

	private List<FuzzySet> getFuzzySets() {
		List<FuzzySet> A = new ArrayList<FuzzySet>();
		A.add(new FuzzySet(Double.NEGATIVE_INFINITY, 0, 0.25));
		A.add(new FuzzySet(0, 0.25, 0.5));
		A.add(new FuzzySet(0.25, 0.5, 0.75));
		A.add(new FuzzySet(0.5, 0.75, 1));
		A.add(new FuzzySet(0.75, 1, Double.POSITIVE_INFINITY));
		return A;
	}
}
