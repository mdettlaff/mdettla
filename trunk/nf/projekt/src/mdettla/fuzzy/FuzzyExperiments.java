package mdettla.fuzzy;

import java.util.List;
import java.util.Random;

public class FuzzyExperiments {

	private static final int RANDOM_POINTS_COUNT = 100;
	private static final int FUZZY_SETS_COUNT = 5;
	private static final double NOISE_LEVEL = 0.05;

	private Random random = new Random();

	public static void main(String[] args) {
		FuzzyExperiments experiments = new FuzzyExperiments();
		experiments.experiment1();
		experiments.experiment2();
	}

	private void experiment1() {
		Function function = new Function() {
			@Override public double evaluate(double x) {
				return Math.sin(x) + noise();
			}
			private double noise() {
				return random.nextDouble() * (2 * NOISE_LEVEL) - NOISE_LEVEL;
			}
		};
		testOneDimensionalFunction(function);
	}

	private void experiment2() {
		Function function = new Function() {
			@Override public double evaluate(double x) {
				double exp = Math.exp(
						-2 * Math.log(2) * Math.pow(
								(x - 0.08) / 0.854,
								2));
				double sin = Math.pow(
						Math.sin(5 * Math.PI * (Math.pow(x, 0.75) - 0.05)),
						6);
				return exp * sin;
			}
		};
		testOneDimensionalFunction(function);
	}

	private void testOneDimensionalFunction(Function function) {
		Range range = new Range(0, 2 * Math.PI);
		List<FuzzySet> fuzzySets =
			FuzzySet.coverRangeWithFuzzySets(range, FUZZY_SETS_COUNT);
		SimpleFuzzyRules fuzzyRules = new SimpleFuzzyRules(
				generateRandomPoints(range, RANDOM_POINTS_COUNT),
				function, fuzzySets);
		double[] testPoints =
			generateRandomPoints(range, RANDOM_POINTS_COUNT);
		double error = fuzzyRules.getError(testPoints);
		System.out.println(error);
	}

	private double[] generateRandomPoints(Range range, int randomPointsCount) {
		double[] randomPoints = new double[randomPointsCount];
		for (int i = 0; i < randomPoints.length; i++) {
			randomPoints[i] = randomPoint(range);
		}
		return randomPoints;
	}

	private double randomPoint(Range range) {
		return range.getBegin() + (random.nextDouble() * range.getWidth());
	}
}
