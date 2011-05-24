package mdettla.experiments;

import java.util.Random;

import mdettla.util.Function;

public class Functions {

	private static final double NOISE_LEVEL = 0.05;
	private static final double NOISE_LEVEL2 = 0.1;
	private static Random random = new Random();

	public static final Function f1 = new Function() {
		@Override public double evaluate(double[] x) {
			return Math.sin(x[0]) + noise();
		}
		private double noise() {
			return random.nextDouble() * (2 * NOISE_LEVEL) - NOISE_LEVEL;
		}
	};

	public static final Function f1b = new Function() {
		@Override public double evaluate(double[] x) {
			return Math.sin(x[0]) + noise();
		}
		private double noise() {
			return random.nextDouble() * (2 * NOISE_LEVEL2) - NOISE_LEVEL2;
		}
	};

	public static final Function f2 = new Function() {
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

	public static final Function f3 = new Function() {
		@Override public double evaluate(double[] x) {
			return 200
			- Math.pow((Math.pow(x[0], 2) + x[1] - 11), 2)
			- (x[0] + Math.pow(x[1], 2) - 7);
		}
	};
}
