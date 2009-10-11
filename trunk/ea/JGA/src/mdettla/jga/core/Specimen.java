package mdettla.jga.core;

public interface Specimen extends Comparable<Specimen>, Cloneable {

	public Specimen createInstance();

	public int getGenotypeLength();

	public Gene getGeneAt(int position);

	public void setGeneAt(int position, Gene gene);

	public Object getFitness();

	/**
	 * @return Reprezentacja napisowa osobnika (fenotyp).
	 */
	public String toString();
}
