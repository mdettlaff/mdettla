package mdettla.ea.zadanie2;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Differential Evolution.
 */
public class GeneticAlgorithmDE {

	public static final double CR = 0.9;
	public static final double F = 0.6;

	/** number of generations */
	private final int generationsCount;
	/** population size */
	private final int mu;
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

	public synchronized static Integer[] range(Integer n) {
		Integer[] range = new Integer[n];
		for (Integer i = 0; i < n; i++) {
			range[i] = i;
		}
		return range;
	}

	private static <T> List<T> randomSample(T[] population, int k) {
		Random random = new Random();
		List<Integer> indexes = new ArrayList<Integer>(population.length);
		for (int i = 0; i < population.length; i++) {
			indexes.add(i);
		}
		List<T> sample = new ArrayList<T>();
		for (int i = 0; i < k; i++) {
			Integer index = indexes.get(random.nextInt(indexes.size()));
			indexes.remove(index);
			sample.add(population[index]);
		}
		return sample;
	}

	private void printInfo(int generation, Specimen[] population) {
		if (generation % 1000 == 0 || generation == generationsCount) {
			Specimen best = Collections.max(Arrays.asList(population));
			System.out.println(
					"Generation: " + generation + " Best Specimen: " + best);
		}
	}

	public void runAlgorithm() {
		java.util.Random random = new java.util.Random();
		/** the population */
		Specimen[] pop = new Specimen[mu];

		System.out.println("Algorytm Genetyczny: Differential Evolution");

		// generate the intial population and calculate fitness
		double sigma = 3.0;
		double[] tt = new double[varsCount];
		for (int j = 0; j < varsCount; j++) {
			tt[j] = (random.nextDouble() - 0.5) * 2.0 * fitness.getArea();
		}
		for (int i = 0; i < mu; i++) {
			pop[i] = new Specimen(varsCount);
			for (int j = 0; j < varsCount; j++) {
				pop[i].vars[j] = tt[j] + sigma * random.nextGaussian();
			}
			pop[i].fitness = fitness.fitness(pop[i]);
		}

		printInfo(0, pop);

		for (int t = 0; t < generationsCount; t++) {
			Specimen[] newPop = new Specimen[mu];
			Specimen[] p = new Specimen[mu];
			for (int i = 0; i < mu; i++) {
				p[i] = new Specimen(varsCount);
			}
			for (int i = 0; i < mu; i++) {
				List<Integer> sample = randomSample(range(mu), 3);
				int r = sample.get(0);
				int u = sample.get(1);
				int v = sample.get(2);
				int jRand = random.nextInt(varsCount);
				for (int j = 0; j < varsCount; j++) {
					if (random.nextDouble() <= CR && j == jRand) {
						p[i].vars[j] = pop[v].vars[j]
								+ F * (pop[r].vars[j] - pop[u].vars[j]);
					} else {
						p[i].vars[j] = pop[i].vars[j];
					}
				}
				p[i].fitness = fitness.fitness(p[i]);
				if (p[i].fitness <= pop[i].fitness) {
					newPop[i] = p[i];
				} else {
					newPop[i] = pop[i];
				}
			}
			pop = newPop;

			printInfo(t + 1, pop);
		}
	}
}
