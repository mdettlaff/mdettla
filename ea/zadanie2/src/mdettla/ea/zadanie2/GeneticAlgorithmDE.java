package mdettla.ea.zadanie2;

/**
 * Differential Evolution.
 */
public class GeneticAlgorithmDE {

	/** number of generations */
	private final int generationsCount;
	/** population size */
	private final int mu;
	/** temporary population size */
	private final int lambda = 100;
	/** number of decision variables */
	private final int varsCount;

	private FitnessFunction fitness;

	public GeneticAlgorithmDE(FitnessFunction fitness,
			int varsCount, int generationsCount, int populationSize) {
		this.fitness = fitness;
		this.varsCount = varsCount;
		this.generationsCount = generationsCount;
		mu = populationSize;
	}

	public void runAlgorithm() {
		/** our source of random numbers */
		java.util.Random rnd = new java.util.Random();
		/** the population */
		Specimen[] pop = new Specimen[mu];
		/** the best Specimen found so far */
		Specimen best = new Specimen(varsCount);
		best.fitness = Double.MAX_VALUE;

		double sigma = 3.0;
		double[] tt = new double[varsCount];

		for (int j = 0; j < varsCount; j++) {
			tt[j] = (rnd.nextDouble() - 0.5) * 2.0 * fitness.getArea();
		}
		// generate the intial population and calculate fitness
		for (int i = 0; i < mu; i++) {
			pop[i] = new Specimen(varsCount);
			for (int j = 0; j < varsCount; j++) {
				pop[i].vars[j] = tt[j] + sigma*rnd.nextGaussian();
			}
			pop[i].fitness = fitness.fitness(pop[i]);
			best = (best.compareTo(pop[i]) < 0) ? best : pop[i];
		}
		System.out.println("Best in initial population:\n" + best);

		System.out.println("Differential Evolution");
	}
}
