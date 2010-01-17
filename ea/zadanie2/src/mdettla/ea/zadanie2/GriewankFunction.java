package mdettla.ea.zadanie2;

/**
 * Griewank's Function - defined for -600.0 < x_i < 600.0
 * minimum at x_i = 0 - this thing is _very_ multimodal
 */
public class GriewankFunction implements FitnessFunction {

	@Override
	public double fitness(Specimen s) {
		double fitness = 0;
		double sum = 0;
		double prod = 1;
        for (int i = 0; i < s.getGenotypeLength(); i++) {
            sum += s.get(i) * s.get(i) / 4000.0;
            prod *= Math.cos(s.get(i) / Math.sqrt(i + 1));
        }
        fitness = 1.0 + sum - prod;
        return fitness;
	}

	@Override
	public int getArea() {
		return 600;
	}
}
