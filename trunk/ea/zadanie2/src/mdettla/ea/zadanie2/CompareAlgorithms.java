package mdettla.ea.zadanie2;

public class CompareAlgorithms {

	public static void main(String[] args) {
		int generationsCount = 1500;
		int populationSize = 15;
		int varsCount = 30;

		new EvolutionaryStrategyCSA(new GriewangkFunction(), varsCount,
				generationsCount, populationSize).runAlgorithm();
	}
}
