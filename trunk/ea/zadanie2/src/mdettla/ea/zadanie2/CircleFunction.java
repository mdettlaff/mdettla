package mdettla.ea.zadanie2;

public class CircleFunction implements FitnessFunction {

	@Override
	public double fitness(Individuum i) {
		double fitness = 0;
		// n-dimensional circle: minimum at x_i = 0
		for (int j = 0; j < i.vars.length; j++) {
			fitness += i.vars[j] * i.vars[j];
		}
		return fitness;
	}

	@Override
	public int getArea() {
		return 600;
	}
}
