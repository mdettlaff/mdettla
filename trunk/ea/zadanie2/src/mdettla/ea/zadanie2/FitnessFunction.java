package mdettla.ea.zadanie2;

public interface FitnessFunction {

	public double fitness(Specimen i);

	/**
	 * Initial variable values are equally distributed from -area to +area.
	 */
	public int getArea();
}
