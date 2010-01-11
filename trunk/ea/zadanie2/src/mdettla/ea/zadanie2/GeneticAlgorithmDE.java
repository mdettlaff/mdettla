package mdettla.ea.zadanie2;

import java.util.ArrayList;
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

	private final FitnessFunction fitness;

	public GeneticAlgorithmDE(FitnessFunction fitness,
			int varsCount, int generationsCount, int populationSize) {
		this.fitness = fitness;
		this.varsCount = varsCount;
		this.generationsCount = generationsCount;
		mu = populationSize;
	}

	private void printInfo(int generation, List<Specimen> population) {
		if (generation % 1000 == 0 || generation == generationsCount) {
			Specimen best = Collections.max(population);
			System.out.println(
					"Generation: " + generation + " Best Specimen: " + best);
		}
	}

	private List<Specimen> generateRandomPopulation() {
		Random random = new Random();
		List<Specimen> population = new ArrayList<Specimen>(mu);
		double sigma = 3.0;
		for (int i = 0; i < mu; i++) {
			Specimen newSpecimen = new Specimen(fitness, varsCount);
			for (int j = 0; j < varsCount; j++) {
				double tt = (random.nextDouble() - 0.5) * 2.0 * fitness.getArea();
				newSpecimen.set(j, tt + sigma * random.nextGaussian());
			}
			population.add(newSpecimen);
		}
		return population;
	}

	public void runAlgorithm() {
		List<Specimen> population;

		System.out.println("Algorytm Genetyczny: Differential Evolution");

		// generate the initial population
		population = generateRandomPopulation();

		printInfo(0, population);

		Random random = new Random();
		for (int t = 0; t < generationsCount; t++) {
			List<Specimen> newPopulation = new ArrayList<Specimen>(mu);
			for (int i = 0; i < mu; i++) {
				List<Specimen> sample = Utils.randomSample(population, 3);
				Specimen r = sample.get(0);
				Specimen u = sample.get(1);
				Specimen v = sample.get(2);
				int jRand = random.nextInt(varsCount);
				Specimen p = new Specimen(fitness, varsCount);
				for (int j = 0; j < varsCount; j++) {
					if (random.nextDouble() <= CR && j == jRand) {
						p.set(j, v.get(j) + F * (r.get(j) - u.get(j)));
					} else {
						p.set(j, population.get(i).get(j));
					}
				}
				// if p[i].fitness <= pop[i].fitness
				if (p.compareTo(population.get(i)) <= 0) {
					newPopulation.add(p);
				} else {
					newPopulation.add(population.get(i));
				}
			}
			population = newPopulation;

			printInfo(t + 1, population);
		}
	}
}
