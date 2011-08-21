package mdettla.keyboard.ga;

public abstract class Objective {

	private Double value;

	abstract String getName();

	public double getValue() {
		if (value == null) {
			value = computeValue();
		}
		return value;
	}

	abstract double computeValue();

	abstract double getWeight();
}
