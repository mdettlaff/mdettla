package mdettla.fuzzy;

import java.util.Arrays;
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
		experiments.experiment3();
	}

	private void experiment1() {
		Function function = new Function() {
			@Override public double evaluate(double[] x) {
				return Math.sin(x[0]) + noise();
			}
			private double noise() {
				return random.nextDouble() * (2 * NOISE_LEVEL) - NOISE_LEVEL;
			}
		};
		testOneDimensionalFunction(function, new Range(0, 2 * Math.PI));
	}

	private void experiment2() {
		Function function = new Function() {
			@Override public double evaluate(double[] x) {
				double exp = Math.exp(
						-2 * Math.log(2) * Math.pow(
								(x[0] - 0.08) / 0.854,
								2));
				double sin = Math.pow(
						Math.sin(5 * Math.PI * (Math.pow(x[0], 0.75) - 0.05)),
						6);
				return exp * sin;
			}
		};
		testOneDimensionalFunction(function, new Range(0, 1));
	}

	private void experiment3() {
		Function function = new Function() {
			@Override public double evaluate(double[] x) {
				return 200
				- Math.pow((Math.pow(x[0], 2) + x[1] - 11), 2)
				- (x[0] + Math.pow(x[1], 2) - 7);
			}
		};
		Range range = new Range(-5, 5);
		List<FuzzySet> fuzzySets =
			FuzzySet.coverRangeWithFuzzySets(range, FUZZY_SETS_COUNT);
		SimpleFuzzyRules fuzzyRules = new SimpleFuzzyRules(
				generateRandom2DPoints(range, RANDOM_POINTS_COUNT),
				function, fuzzySets);
		double[][] testPoints =
			generateRandom2DPoints(range, RANDOM_POINTS_COUNT);
		double error = fuzzyRules.getError(testPoints);
		System.out.println(error);
	}

	private void testOneDimensionalFunction(Function function, Range range) {
		List<FuzzySet> fuzzySets =
			FuzzySet.coverRangeWithFuzzySets(range, FUZZY_SETS_COUNT);
		SimpleFuzzyRules fuzzyRules = new SimpleFuzzyRules(
				to2DArray(generateRandomPoints(range, RANDOM_POINTS_COUNT)),
				function, fuzzySets);
		double[] testPoints =
			generateRandomPoints(range, RANDOM_POINTS_COUNT);
		double error = fuzzyRules.getError(to2DArray(testPoints));
		System.out.println(error);
	}

	double[][] to2DArray(double[] xs) {
		double[][] result = new double[xs.length][];
		for (int i = 0; i < xs.length; i++) {
			result[i] = new double[] {xs[i]};
		}
		return result;
	}

	private double[] generateRandomPoints(Range range, int randomPointsCount) {
		double[] randomPoints = new double[randomPointsCount];
		for (int i = 0; i < randomPoints.length; i++) {
			randomPoints[i] = randomPoint(range);
		}
		Arrays.sort(randomPoints);
		return randomPoints;
	}

	private double[][] generateRandom2DPoints(Range range, int randomPointsCount) {
		double[] xs = generateRandomPoints(range, randomPointsCount);
		double[] ys = generateRandomPoints(range, randomPointsCount);
		double[][] result = new double[randomPointsCount][];
		for (int i = 0; i < xs.length; i++) {
			result[i] = new double[] {xs[i], ys[i]};
		}
		return result;
	}

	private double randomPoint(Range range) {
		return range.getBegin() + (random.nextDouble() * range.getWidth());
	}
}
