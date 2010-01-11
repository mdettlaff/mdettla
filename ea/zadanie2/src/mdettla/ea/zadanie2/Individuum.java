package mdettla.ea.zadanie2;

/** this class represents one Individuum */
public class Individuum implements Comparable<Individuum> {

	/** number of rotation angles for correlated mutation */
//	private int numangles;
	/** the decision variables */
	public double[] vars;
	/** the standard deviations */
	public double[] sdevs;
	/** the correlation angles */
//	public double[] angles;
	/** the fitness value */
	public double fitness;

	public Individuum(int numvars) {
//		numangles = numvars * (numvars - 1) / 2;
		vars = new double[numvars];
		sdevs = new double[numvars];
//		angles = new double[numangles];
	}

	/** compare this Individuum to another looking only at the fitness */
	public int compareTo(Individuum i) {
		if (this.fitness == i.fitness) {
			return 0;
		} else {
			return (this.fitness < i.fitness) ? -1 : 1;
		}
	}

	/** return String representation of all values */
	public String toString() {
		//sdevstr = "", anglestr = "";
		String varstr = String.format("fitness: %.5f vars: ", this.fitness);
		for (int i = 0; i < vars.length; i++) {
			varstr += String.format("%.3f ", this.vars[i]);
		}
		return varstr.substring(0, varstr.length() - 1);
	}
}
