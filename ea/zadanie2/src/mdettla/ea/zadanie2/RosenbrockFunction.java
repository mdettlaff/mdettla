package mdettla.ea.zadanie2;

/**
 * Rosenbrock's Saddle Function - defined for -2.048 < x_i < 2.048
 * minimum at x_i = 1.0
 */
public class RosenbrockFunction implements FitnessFunction {

	@Override
	public double fitness(Specimen i) {
		double fitness = 0;
		for (int j = 1; j < i.vars.length; j++) {
			if (Math.abs(i.vars[j]) > 2048) return Double.MAX_VALUE; // penalty
			fitness += 100.0 * (i.vars[j] - i.vars[j-1] * i.vars[j-1])
				* (i.vars[j] - i.vars[j-1] * i.vars[j-1])
				+ (1 - i.vars[j-1])*(1 - i.vars[j-1]);
		}
		return fitness;
	}

	@Override
	public int getArea() {
		return 30;
	}
}
