package mdettla.ea.zadanie2;

/**
 * Represents one specimen.
 */
public class Specimen implements Comparable<Specimen> {

	/** the decision variables */
	private double[] vars;
	private FitnessFunction fitnessFunction;
	private Double fitness;

	public Specimen(FitnessFunction fitnessFunction, int varsCount) {
		vars = new double[varsCount];
		this.fitnessFunction = fitnessFunction;
	}

	public double get(int i) {
		return vars[i];
	}

	public void set(int i, double value) {
		vars[i] = value;
	}

	public int getGenotypeLength() {
		return vars.length;
	}

	public double getFitness() {
		if (fitness == null) {
			fitness = fitnessFunction.fitness(this);
		}
		return fitness;
	}

	/**
	 * Compare this Specimen to another looking only at the fitness.
	 */
	@Override
	public int compareTo(Specimen other) {
		return Double.compare(getFitness(), other.getFitness());
	}

	@Override
	public String toString() {
		String s = String.format("fitness: %.5f vars: ", this.getFitness());
		for (int i = 0; i < vars.length; i++) {
			s += String.format("%.3f ", this.vars[i]);
		}
		return s.substring(0, s.length() - 1);
	}
}
