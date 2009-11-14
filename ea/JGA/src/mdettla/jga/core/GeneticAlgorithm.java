package mdettla.jga.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import mdettla.jga.operators.crossover.CutPointCrossover;
import mdettla.jga.operators.mutation.OnePointMutation;
import mdettla.jga.operators.selection.TournamentSelection;

/**
 * Algorytm genetyczny.
 * <p>
 * Aby zastosować algorytm genetyczny do danego problemu, należy:<br>
 * <ol>
 *   <li>
 *     Utworzyć klasę implementującą interfejs {@link Specimen}, której
 *     instancje będą osobnikami w algorytmie. Interfejs ten umożliwia
 *     między innymi określenie funkcji przystosowania.
 *   </li>
 *   <li>
 *     Stworzyć nowy obiekt poniższej klasy, jako argument konstruktora
 *     podając populację początkową. Populacja początkowa najczęściej składa
 *     się z osobników z losowo utworzonym genotypem.
 *   </li>
 *   <li>
 *     Ustawić parametry algorytmu (operatory genetyczne itp.) za pomocą metod
 *     tej klasy, a następnie uruchomić algorytm za pomocą metody
 *     {@link GeneticAlgorithm#runEpoch(int) runEpoch}. Metoda ta zwróci
 *     najlepiej przystosowanego osobnika znalezionego przez algorytm.
 *   </li>
 * </ol>
 */
public class GeneticAlgorithm {

	public static final double DEFAULT_CROSSOVER_PROBABILITY = .7;
	public static final double DEFAULT_MUTATION_PROBABILITY = .7;
	public static final int DEFAULT_THREAD_POOL_SIZE = 7;

	private List<Specimen> initialPopulation;
	private CrossoverOperator crossoverOperator;
	private double crossoverProbability;
	private MutationOperator mutationOperator;
	private double mutationProbability;
	private SelectionFunction selectionFunction;
	private int populationSize;
	private int threadPoolSize;
	private boolean printResults;
	private List<Specimen> lastPopulation;

	public GeneticAlgorithm(List<Specimen> initialPopulation) {
		this.initialPopulation = initialPopulation;
		populationSize = initialPopulation.size();
		crossoverOperator = new CutPointCrossover(1);
		crossoverProbability = DEFAULT_CROSSOVER_PROBABILITY;
		mutationOperator = new OnePointMutation();
		mutationProbability = DEFAULT_MUTATION_PROBABILITY;
		selectionFunction = new TournamentSelection(4);
		threadPoolSize = DEFAULT_THREAD_POOL_SIZE;
		printResults = false;
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

	public void setPrintResults(boolean printResults) {
		this.printResults = printResults;
	}

	public List<Specimen> getLastPopulation() {
		return lastPopulation;
	}

	/**
	 * Jeden przebieg algorytmu genetycznego.
	 *
	 * @param generations   Ilość pokoleń (iteracji) algorytmu.
	 * @return              Najlepszy znaleziony osobnik.
	 */
	public Specimen runEpoch(int generations) {
		Specimen best = null;
		try {
			List<Specimen> bestFromEachPopulation = new ArrayList<Specimen>();
			List<Specimen> population = initialPopulation;

			info();
			for (int i = 0; i < generations; i++) {
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
				Specimen bestInPopulation = Collections.max(population);
				bestFromEachPopulation.add(bestInPopulation);

				info(i, bestInPopulation);
			}
			lastPopulation = population;
			best = Collections.max(bestFromEachPopulation);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return best;
	}

	private void info() {
		if (printResults) {
			System.out.println("Najlepsze przystosowanie w kolejnych pokoleniach:");
		}
	}

	private void info(int generation, Specimen bestInPopulation) {
		if (printResults) {
			System.out.println((generation + 1) + "\t" +
					bestInPopulation.getFitness().toString());
		}
	}
}
