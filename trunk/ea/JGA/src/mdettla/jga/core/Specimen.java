package mdettla.jga.core;


public interface Specimen extends Comparable<Specimen> {

	public Specimen createCopy();

	public int getGenotypeLength();

	public Object getGeneAt(int position);

	public void setGeneAt(int position, Object gene);

	public void setRandomGeneValueAt(int position);

	public void setOppositeGeneValueAt(int position);

	public Number getFitness();
}
