package mdettla.ea.zadanie2;

/**
 * Rosenbrock's Saddle Function - defined for -2048 < x_i < 2048
 * minimum at x_i = 1.0
 */
public class RosenbrockFunction implements FitnessFunction {

	@Override
	public double fitness(Specimen s) {
		double fitness = 0;
		for (int i = 1; i < s.getGenotypeLength(); i++) {
			if (Math.abs(s.get(i)) > 2048) return Double.MAX_VALUE; // penalty
			fitness += 100.0 * (s.get(i) - s.get(i-1) * s.get(i-1))
				* (s.get(i) - s.get(i-1) * s.get(i-1))
				+ (1 - s.get(i-1))*(1 - s.get(i-1));
		}
		return fitness;
	}

	@Override
	public int getArea() {
		return 30;
	}
}
