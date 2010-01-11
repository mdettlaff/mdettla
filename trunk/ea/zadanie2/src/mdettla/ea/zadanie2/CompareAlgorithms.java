package mdettla.ea.zadanie2;

public class CompareAlgorithms {

	public static void main(String[] args) {
		int generationsCount = 1000;
		int populationSize = 15;

		EvolutionaryStrategyCSA csa = new EvolutionaryStrategyCSA(
				new CircleFunction(), 20,
				generationsCount, populationSize);
		csa.runAlgorithm();
	}
}
