package mdettla.jga.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

/**
 * Przykładowy osobnik do testowania operatorów genetycznych.
 */
public class Sequence implements Specimen {

	private static Random random = new Random();

	private List<Integer> genotype;

	public Sequence(List<Integer> genotype) {
		this.genotype = genotype;
	}

	@Override
	public Specimen createCopy() {
		return new Sequence(new ArrayList<Integer>(genotype));
	}

	@Override
	public void computeFitness() {
	}

	@Override
	public Number getFitness() {
		return 0;
	}

	@Override
	public Object getGeneAt(int position) {
		return genotype.get(position);
	}

	@Override
	public int getGenotypeLength() {
		return genotype.size();
	}

	@Override
	public void setGeneAt(int position, Object gene) {
		genotype.set(position, (Integer)gene);
	}

	@Override
	public void setOppositeGeneValueAt(int position) {
		genotype.set(position, random.nextInt());
	}

	@Override
	public void setRandomGeneValueAt(int position) {
		genotype.set(position, random.nextInt());
	}

	@Override
	public int compareTo(Specimen o) {
		return 0;
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof Sequence)) {
			return false;
		}
		Sequence other = (Sequence)obj;
		return genotype.equals(other.genotype);
	}

	@Override
	public String toString() {
		return "Sequence: " + genotype;
	}

	@Override
	public String getPhenotype() {
		return genotype.toString();
	}
}
