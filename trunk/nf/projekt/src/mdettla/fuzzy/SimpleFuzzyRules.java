package mdettla.fuzzy;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SimpleFuzzyRules {

	private static final double ALPHA = 10;

	private final Map<FuzzySet, Double> rules;

	public SimpleFuzzyRules(double[][] data, List<FuzzySet> fuzzySets) {
		rules = generateRules(data, fuzzySets);
	}

	private Map<FuzzySet, Double> generateRules(
			double[][] data, List<FuzzySet> fuzzySets) {
		Map<FuzzySet, Double> rules = new LinkedHashMap<FuzzySet, Double>();
		double sum;
		for (FuzzySet fuzzySet : fuzzySets) {
			sum = 0;
			double b = 0;
			for (int j = 0; j < data.length; j++) {
				double x = data[j][0];
				double y = data[j][1];
				double membership = fuzzySet.membership(x);
				double w = Math.pow(membership, ALPHA);
				sum += w;
				b += w * y;
			}
			b /= sum;
			rules.put(fuzzySet, b);
		}
		return rules;
	}

	public double getOutput(double x) {
		double nominator = 0;
		double denominator = 0;
		for (Map.Entry<FuzzySet, Double> rule : rules.entrySet()) {
			FuzzySet fuzzySet = rule.getKey();
			double b = rule.getValue();
			double membership = fuzzySet.membership(x);
			nominator += membership * b;
			denominator += membership;
		}
		return nominator / denominator;
	}
}
