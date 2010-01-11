package mdettla.ea.zadanie2;

/**
 * This class represents one specimen.
 */
public class Specimen implements Comparable<Specimen> {

	/** the decision variables */
	public double[] vars;
	/** the standard deviations */
	public double[] sdevs;
	/** the fitness value */
	public double fitness;

	public Specimen(int varsCount) {
		vars = new double[varsCount];
		sdevs = new double[varsCount];
	}

	/** compare this Specimen to another looking only at the fitness */
	public int compareTo(Specimen specimen) {
		if (this.fitness == specimen.fitness) {
			return 0;
		} else {
			return (this.fitness < specimen.fitness) ? -1 : 1;
		}
	}

	public String toString() {
		String varstr = String.format("fitness: %.5f vars: ", this.fitness);
		for (int i = 0; i < vars.length; i++) {
			varstr += String.format("%.3f ", this.vars[i]);
		}
		return varstr.substring(0, varstr.length() - 1);
	}
}
