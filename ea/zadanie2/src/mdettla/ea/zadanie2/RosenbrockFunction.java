package mdettla.ea.zadanie2;

/**
 * Rosenbrock's Saddle Function - defined for -2.048 < x_i < 2.048
 * minimum at x_i = 1.0
 */
public class RosenbrockFunction implements FitnessFunction {

	@Override
	public double fitness(Specimen s) {
		double fitness = 0;
		for (int i = 1; i < s.vars.length; i++) {
			if (Math.abs(s.vars[i]) > 2048) return Double.MAX_VALUE; // penalty
			fitness += 100.0 * (s.vars[i] - s.vars[i-1] * s.vars[i-1])
				* (s.vars[i] - s.vars[i-1] * s.vars[i-1])
				+ (1 - s.vars[i-1])*(1 - s.vars[i-1]);
		}
		return fitness;
	}

	@Override
	public int getArea() {
		return 30;
	}
}
