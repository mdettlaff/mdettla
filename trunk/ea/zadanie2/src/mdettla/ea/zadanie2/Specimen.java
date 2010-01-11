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

	public Specimen(int numvars) {
		vars = new double[numvars];
		sdevs = new double[numvars];
	}

	/** compare this Specimen to another looking only at the fitness */
	public int compareTo(Specimen i) {
		if (this.fitness == i.fitness) {
			return 0;
		} else {
			return (this.fitness < i.fitness) ? -1 : 1;
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
