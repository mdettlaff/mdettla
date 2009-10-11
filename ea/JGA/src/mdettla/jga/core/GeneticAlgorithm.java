package mdettla.jga.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

/**
 * Algorytm genetyczny.
 */
public class GeneticAlgorithm {

	public static final int DEFAULT_POPULATION_SIZE = 100;
	public static final double DEFAULT_MUTATION_PROBABILITY = .7;
	public static final double DEFAULT_CROSSOVER_PROBABILITY = .7;

	private Class<? extends Specimen> specimenClass;
	private CrossoverOperator crossoverOperator;
	private double crossoverProbability;
	private MutationOperator mutationOperator;
	private double mutationProbability;
	private SelectionFunction selectionFunction;
	private int populationSize;
	private int threadPoolSize;

	public GeneticAlgorithm(Class<? extends Specimen> specimenClass) {
		this.specimenClass = specimenClass;
		mutationProbability = DEFAULT_MUTATION_PROBABILITY;
		crossoverProbability = DEFAULT_CROSSOVER_PROBABILITY;
		populationSize = DEFAULT_POPULATION_SIZE;
		threadPoolSize = 1;
	}

	public void setCrossoverOperator(CrossoverOperator crossoverOperator) {
		this.crossoverOperator = crossoverOperator;
	}

	public CrossoverOperator getCrossoverOperator() {
		return crossoverOperator;
	}

	public void setCrossoverProbability(double crossoverProbability) {
		if (crossoverProbability < 0 || crossoverProbability > 1) {
			throw new IllegalArgumentException("Prawdopodobieństwo " +
					"krzyżowania musi być wartością z zakresu [0, 1].");
		}
		this.crossoverProbability = crossoverProbability;
	}

	public double getCrossoverProbability() {
		return crossoverProbability;
	}

	public void setMutationOperator(MutationOperator mutationOperator) {
		this.mutationOperator = mutationOperator;
	}

	public MutationOperator getMutationOperator() {
		return mutationOperator;
	}

	public void setMutationProbability(double mutationProbability) {
		if (mutationProbability < 0 || mutationProbability > 1) {
			throw new IllegalArgumentException("Prawdopodobieństwo mutacji " +
					"musi być wartością z zakresu [0, 1].");
		}
		this.mutationProbability = mutationProbability;
	}

	public double getMutationProbability() {
		return mutationProbability;
	}

	public void setSelectionFunction(SelectionFunction selectionFunction) {
		this.selectionFunction = selectionFunction;
	}

	public SelectionFunction getSelectionFunction() {
		return selectionFunction;
	}

	public void setThreadPoolSize(int threadPoolSize) {
		this.threadPoolSize = Math.max(threadPoolSize, 1);
	}

	public void runEpoch(int iterations) throws JGAException {
		try {
			List<Specimen> population = Collections.synchronizedList(
					new ArrayList<Specimen>(populationSize));

			System.out.println("Najlepsze przystosowanie w kolejnych populacjach:");
			Random random = new Random();
			for (int i = 0; i < populationSize; i++) {
				population.add(createRandomSpecimen(random));
			}
			for (int i = 0; i < iterations; i++) {
				List<Specimen> newPopulation = Collections.synchronizedList(
						new ArrayList<Specimen>(populationSize));
				List<Thread> threadPool = new ArrayList<Thread>();
				for (int j = 0; j < threadPoolSize; j++) {
					int specimens = populationSize / threadPoolSize;
					if (j == 0) {
						specimens += populationSize % threadPoolSize;
					}
					Thread thread = new SpecimenCreatorThread(
							this, specimens, newPopulation, population);
					thread.start();
					threadPool.add(thread);
				}
				for (int j = 0; j < threadPool.size(); j++) {
					threadPool.get(j).join();
				}
				population = newPopulation;
				System.out.print((i + 1) + "\t" +
						Collections.max(population).getFitness().toString());
			}
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	private Specimen createRandomSpecimen(Random random) throws JGAException {
		Specimen randomSpecimen;
		try {
			randomSpecimen =
				specimenClass.newInstance().createInstance();
			for (int i = 0; i < randomSpecimen.getGenotypeLength(); i++) {
				Gene gene = randomSpecimen.getGeneAt(i);
				gene.setRandomValue(random);
			}
		} catch (InstantiationException e) {
			throw new JGAException("Nie można utworzyć obiektu klasy " +
					specimenClass);
		} catch (IllegalAccessException e) {
			throw new JGAException("Błąd dostępu w klasie " + specimenClass);
		}
		return randomSpecimen;
	}
}
