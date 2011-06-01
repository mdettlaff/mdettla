package mdettla.fuzzy;

import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import mdettla.util.Function;

public class SimpleFuzzyRules3D {

	private static final double ALPHA = 10;

	private final double[][] dataIn;
	private final Function function;
	private final double[] dataOut;
	private final Map<List<FuzzySet>, Double> rules;

	public SimpleFuzzyRules3D(
			double[][] dataIn, Function function, List<FuzzySet> fuzzySets) {
		this.function = function;
		this.dataIn = dataIn;
		dataOut = getDataOutputs();
		rules = generateRules(fuzzySets);
	}

	private double[] getDataOutputs() {
		double[] dataOut = new double[dataIn.length];
		for (int i = 0; i < dataIn.length; i++) {
			dataOut[i] = function.evaluate(dataIn[i]);
		}
		return dataOut;
	}

	private Map<List<FuzzySet>, Double> generateRules(List<FuzzySet> fuzzySets) {
		Map<List<FuzzySet>, Double> rules = new LinkedHashMap<List<FuzzySet>, Double>();
		double sum;
		for (FuzzySet fuzzySet1 : fuzzySets) {
			for (FuzzySet fuzzySet2 : fuzzySets) {
				sum = 0;
				double b = 0;
				for (int j = 0; j < dataIn.length; j++) {
					double x1 = dataIn[j][0];
					double x2 = dataIn[j][1];
					double membershipMul = fuzzySet1.membership(x1) * fuzzySet2.membership(x2);
					double w = Math.pow(membershipMul, ALPHA);
					sum += w;
					double y = dataOut[j];
					b += w * y;
				}
				b = sum == 0 ? 0 : b / sum;
				rules.put(Arrays.asList(fuzzySet1, fuzzySet2), b);
			}
		}
		return rules;
	}

	public double getOutput(double[] x) {
		double nominator = 0;
		double denominator = 0;
		for (Map.Entry<List<FuzzySet>, Double> rule : rules.entrySet()) {
			List<FuzzySet> fuzzySets = rule.getKey();
			double b = rule.getValue();
			double membershipMul = 1;
			for (int i = 0; i < x.length; i++) {
				double membership = fuzzySets.get(i).membership(x[i]);
				membershipMul *= membership;
			}
			nominator += membershipMul * b;
			denominator += membershipMul;
		}
		return nominator / denominator;
	}

	public double getError(double[][] xs) {
		double error = 0;
		for (double[] x : xs) {
			double expected = function.evaluate(x);
			double actual = getOutput(x);
			error += Math.pow(actual - expected, 2);
		}
		return Math.sqrt(error);
	}
}
