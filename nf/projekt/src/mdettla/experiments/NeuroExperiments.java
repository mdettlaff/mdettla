package mdettla.experiments;

import java.util.Arrays;
import java.util.Random;

import mdettla.neuro.BackPropagation;
import mdettla.util.Function;
import mdettla.util.Range;

public class NeuroExperiments {

	private static final int RANDOM_POINTS_COUNT = 100;

	private Random random = new Random();

	public static void main(String[] args) {
		System.out.println("Back propagation experiments");
		NeuroExperiments experiments = new NeuroExperiments();
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
		testOneDimensionalFunction(Functions.f1, new Range(0, 2 * Math.PI));
	}

	private void experiment2() {
		testOneDimensionalFunction(Functions.f2, new Range(0, 1));
	}

	private void experiment3() {
		Function function = Functions.f3;
		Range range = new Range(-5, 5);
		BackPropagation backPropagation = new BackPropagation(
				generateRandom2DPoints(range, RANDOM_POINTS_COUNT),
				function);
		double[][] testPoints =
			generateRandom2DPoints(range, RANDOM_POINTS_COUNT);
		printExperimentResults(function, backPropagation, testPoints);
	}

	private void testOneDimensionalFunction(Function function, Range range) {
		BackPropagation backPropagation = new BackPropagation(
				to2DArray(generateRandomPoints(range, RANDOM_POINTS_COUNT)),
				function);
		double[][] testPoints =
			to2DArray(generateRandomPoints(range, RANDOM_POINTS_COUNT));
		printExperimentResults(function, backPropagation, testPoints);
	}

	private void printExperimentResults(Function function,
			BackPropagation backPropagation, double[][] testPoints) {
		for (double[] xs : testPoints) {
			double output = backPropagation.getOutput(xs);
			System.out.println(String.format(
					"f(" + Arrays.toString(xs) + ") = %.2f, expected %.2f",
					output, function.evaluate(xs)));
		}
		double error = backPropagation.getError(testPoints);
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
