package mdettla.ea.zadanie2;

/**
 * Griewangk's Function - defined for -600.0 < x_i < 600.0
 * minimum at x_i = 0 - this thing is _very_ multimodal
 */
public class GriewangkFunction implements FitnessFunction {

	@Override
	public double fitness(Specimen i) {
		double fitness = 0;
		double sum = 0;
		double prod = 1;
        for (int j = 0; j < i.vars.length; j++) {
            sum += i.vars[j] * i.vars[j] / 4000.0;
            prod *= Math.cos((i.vars[j]) / Math.sqrt(j + 1));
        }
        fitness = 1.0 + sum - prod;
        return fitness;
	}

	@Override
	public int getArea() {
		return 600;
	}
}
