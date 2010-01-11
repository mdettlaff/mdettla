package mdettla.ea.zadanie2;

import java.util.Arrays;
import java.util.Collections;

/**
 * Cumulative Step Adaptation.
 * http://www.informatik.uni-ulm.de/ni/Lehre/SS02/Evosem/es.java
 */
public class EvolutionaryStrategyCSA {

	/** number of generations */
	private final int generationsCount;
	/** population size */
	private final int mu;
	/** temporary population size */
	private final int lambda = 100;
	/** number of decision variables */
	private final int varsCount;

	private FitnessFunction fitness;

	public EvolutionaryStrategyCSA(FitnessFunction fitness,
			int varsCount, int generationsCount, int populationSize) {
		this.fitness = fitness;
		this.varsCount = varsCount;
		this.generationsCount = generationsCount;
		mu = populationSize;
	}

	private void printInfo(int generation, Specimen[] population, double sigma) {
		if (generation % 500 == 0 || generation == generationsCount) {
			Specimen best = Collections.max(Arrays.asList(population));
			System.out.println(
					"Generation: " + generation + " Best Specimen: " + best +
					String.format(" sigma: %.5f", sigma));
		}
	}

	public void runAlgorithm() {
		/** our source of random numbers */
		java.util.Random rnd = new java.util.Random();
		/** the population */
		Specimen[] pop = new Specimen[mu];

		double sigma = 3.0;
		double chi_n = Math.sqrt(varsCount)*(1 - 1.0/(4.0*varsCount) +
				1.0/(21.0*varsCount*varsCount));
		double dump = Math.sqrt(varsCount);
		double c = 1/dump;
		double mult1 = 1-c;
		double mult2 = Math.sqrt(c*(2-c)*mu);

		System.out.println("Strategia Ewolucyjna: Cumulative Step Adaptation");

		double[] tt = new double[varsCount];
		for (int j = 0; j < varsCount; j++) {
			tt[j] = (rnd.nextDouble() - 0.5) * 2.0 * fitness.getArea();
		}
		// generate the intial population and calculate fitness
		for (int i = 0; i < mu; i++) {
			pop[i] = new Specimen(fitness, varsCount);
			for (int j = 0; j < varsCount; j++) {
				pop[i].set(j, tt[j] + sigma * rnd.nextGaussian());
			}
		}
		printInfo(0, pop, sigma);

		double[] x_old = new double[varsCount];
		double[] x_new = new double[varsCount];
		double[] path  = new double[varsCount];

		// implementacja r-nania (6.63)
		for (int j = 0; j < varsCount; j++) {
			x_old[j] = 0;
			for (int k = 0; k < mu; k++) {
				x_old[j] += pop[k].get(j);
			}
			x_old[j] = x_old[j] / mu;
		} 

		// the generation loop
		for (int i = 1; i <= generationsCount; i++) {
			/** the temporary population */
			Specimen[] tpop = new Specimen[lambda + mu];

			for (int j = 0; j < lambda; j++) {

				Specimen child = new Specimen(fitness, varsCount);

				for (int k = 0; k < varsCount; k++) {
					child.set(k, x_old[k] + sigma * rnd.nextGaussian());
				}
				// evaluate the child
				tpop[j] = child;
			}

			for (int j = 0; j < mu; j++) {
				tpop[lambda + j] = pop[j];
			}

			Arrays.sort(tpop); // sorts ascending

			for (int j = 0; j < mu; j++) {
				pop[j] = tpop[j];
			}

			// wyznaczenie nowej średniej
			for (int j = 0; j < varsCount; j++) {
				x_new[j] = 0;
				for (int k = 0; k < mu; k++) {
					x_new[j] += pop[k].get(j);
				}
				x_new[j] = x_new[j] / mu;
			}

			// modyfikacja ścieżki
			double d = 0;
			for (int j = 0; j < varsCount; j++) {
				path[j] = path[j]*mult1;
				path[j] += mult2 / Math.sqrt(sigma) * (x_new[j] - x_old[j]);
				d = d + path[j]*path[j];
				x_old[j] = x_new[j];
			}

			d = Math.sqrt(d);
			d = (d-chi_n)/dump/chi_n;
			// nowe sigma
			sigma = sigma * Math.exp(d);
			if (sigma < 0.0001) {
				sigma = 0.0001;
			} else if (sigma > 100) {
				sigma = 100;
			}

			printInfo(i, pop, sigma);
		}
	}
}
