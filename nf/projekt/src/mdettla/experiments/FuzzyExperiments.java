package mdettla.experiments;

import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import mdettla.fuzzy.FuzzySet;
import mdettla.fuzzy.SimpleFuzzyRules;
import mdettla.fuzzy.SimpleFuzzyRules3D;
import mdettla.util.FileUtils;
import mdettla.util.Function;
import mdettla.util.Range;
import mdettla.util.StringUtils;

public class FuzzyExperiments {

	private static final int RANDOM_POINTS_COUNT = 100;
	private static final int FUZZY_SETS_COUNT = 10;

	private Random random = new Random();

	public static void main(String[] args) {
		System.out.println("Fuzzy experiments");
		FuzzyExperiments experiments = new FuzzyExperiments();
		System.out.println();
		System.out.println("Experiment 1");
		experiments.experiment1();
		System.out.println();
		System.out.println("Experiment 2");
		experiments.experiment2();
		System.out.println();
		System.out.println("Experiment 3");
		experiments.experiment3();
	}

	private void experiment1() {
		testOneDimensionalFunction(Functions.f1, new Range(0, 2 * Math.PI), "f1");
		testOneDimensionalFunction(Functions.f1b, new Range(0, 2 * Math.PI), "f1b");
	}

	private void experiment2() {
		testOneDimensionalFunction(Functions.f2, new Range(0, 1), "f2");
	}

	private void experiment3() {
		Function function = Functions.f3;
		Range range = new Range(-5, 5);
		int fuzzySetsCount = 10;
		List<FuzzySet> fuzzySets =
			FuzzySet.coverRangeWithFuzzySets(range, fuzzySetsCount);
		SimpleFuzzyRules3D fuzzyRules = new SimpleFuzzyRules3D(
				generateRandom2DPoints(range, RANDOM_POINTS_COUNT),
				function, fuzzySets);
		double[][] testPoints =
			generateRandom2DPoints(range, RANDOM_POINTS_COUNT);

		String filename = "f3";
		StringBuilder expected = new StringBuilder();
		StringBuilder result = new StringBuilder();
		for (double[] xs : testPoints) {
			double output = fuzzyRules.getOutput(xs);
//			System.out.println(String.format(
//					"f(" + Arrays.toString(xs) + ") = %.2f, expected %.2f",
//					output, function.evaluate(xs)));
			expected.append(StringUtils.join(xs, " ") + " " + output + "\n");
			result.append(StringUtils.join(xs, " ") + " " + function.evaluate(xs) + "\n");
		}
		FileUtils.writeStringToFile(expected.toString(),
				new File("doc/fuzzy_" + filename + "_expected.dat"));
		FileUtils.writeStringToFile(result.toString(),
				new File("doc/fuzzy_" + filename + "_result.dat"));
		double error = fuzzyRules.getError(testPoints);
		System.out.println("error = " + error);
	}

	private void testOneDimensionalFunction(Function function, Range range,
			String filename) {
		List<FuzzySet> fuzzySets =
			FuzzySet.coverRangeWithFuzzySets(range, FUZZY_SETS_COUNT);
		SimpleFuzzyRules fuzzyRules = new SimpleFuzzyRules(
				to2DArray(generateRandomPoints(range, RANDOM_POINTS_COUNT)),
				function, fuzzySets);
		double[][] testPoints =
			to2DArray(generateRandomPoints(range, RANDOM_POINTS_COUNT));
		printExperimentResults(function, fuzzyRules, testPoints, filename);
	}

	private void printExperimentResults(Function function,
			SimpleFuzzyRules fuzzyRules, double[][] testPoints, String filename) {
		StringBuilder expected = new StringBuilder();
		StringBuilder result = new StringBuilder();
		for (double[] xs : testPoints) {
			double output = fuzzyRules.getOutput(xs);
//			System.out.println(String.format(
//					"f(" + Arrays.toString(xs) + ") = %.2f, expected %.2f",
//					output, function.evaluate(xs)));
			expected.append(StringUtils.join(xs, " ") + " " + output + "\n");
			result.append(StringUtils.join(xs, " ") + " " + function.evaluate(xs) + "\n");
		}
		FileUtils.writeStringToFile(expected.toString(),
				new File("doc/fuzzy_" + filename + "_expected.dat"));
		FileUtils.writeStringToFile(result.toString(),
				new File("doc/fuzzy_" + filename + "_result.dat"));
		double error = fuzzyRules.getError(testPoints);
		System.out.println("error = " + error);
	}

	double[][] to2DArray(double[] xs) {
		double[][] result = new double[xs.length][];
		for (int i = 0; i < xs.length; i++) {
			result[i] = new double[] {xs[i]};
		}
		return result;
	}

	private double[] generateRandomPoints(Range range, int randomPointsCount) {
		return generateRandomPoints(range, randomPointsCount, true);
	}

	private double[] generateRandomPoints(Range range, int randomPointsCount, boolean sort) {
		double[] randomPoints = new double[randomPointsCount];
		for (int i = 0; i < randomPoints.length; i++) {
			randomPoints[i] = randomPoint(range);
		}
		if (sort) {
			Arrays.sort(randomPoints);
		}
		return randomPoints;
	}

	private double[][] generateRandom2DPoints(Range range, int randomPointsCount) {
		double[] xs = generateRandomPoints(range, randomPointsCount);
		double[] ys = generateRandomPoints(range, randomPointsCount, false);
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
