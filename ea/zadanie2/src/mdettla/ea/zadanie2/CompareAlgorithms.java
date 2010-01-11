package mdettla.ea.zadanie2;

public class CompareAlgorithms {

	public static void main(String[] args) {
		int populationSize = 15;
		int varsCount = 30;

		System.out.println("Funkcja Rosenbrocka\n");
		new GeneticAlgorithmDE(new RosenbrockFunction(), varsCount,
				20000, populationSize).runAlgorithm();
		System.out.println();
		new EvolutionaryStrategyCSA(new RosenbrockFunction(), varsCount,
				10000, populationSize).runAlgorithm();

		System.out.println("\n\nFunkcja Griewangka\n");
		new GeneticAlgorithmDE(new GriewangkFunction(), varsCount,
				10000, populationSize).runAlgorithm();
		System.out.println();
		new EvolutionaryStrategyCSA(new GriewangkFunction(), varsCount,
				1500, populationSize).runAlgorithm();
	}
}
