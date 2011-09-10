package mdettla.jga.core;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;

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
 *     tej klasy. Parametry, których nie ustawimy, będą miały wartości
 *     domyślne.
 *   </li>
 *   <li>
 *     Uruchomić algorytm za pomocą metody {@link #runEpoch(int) runEpoch}.
 *     Metoda ta zwróci najlepiej przystosowanego osobnika znalezionego przez
 *     algorytm.
 *   </li>
 * </ol>
 */
public class GeneticAlgorithm {

	private List<Specimen> initialPopulation;
	private CrossoverOperator crossoverOperator;
	private double crossoverProbability;
	private MutationOperator mutationOperator;
	private double mutationProbability;
	private SelectionFunction selectionFunction;
	protected int populationSize;
	private int eliteSize;
	private boolean quiet;
	private final Random random;

	public GeneticAlgorithm(List<Specimen> initialPopulation) {
		this.initialPopulation = initialPopulation;
		populationSize = initialPopulation.size();
		crossoverOperator = new CutPointCrossover(1);
		crossoverProbability = .7;
		mutationOperator = new OnePointMutation();
		mutationProbability = .7;
		selectionFunction = new TournamentSelection(4);
		eliteSize = 0;
		random = new Random();
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

	public void setEliteSize(int eliteSize) {
		this.eliteSize = eliteSize;
	}

	public int getEliteSize() {
		return eliteSize;
	}

	public boolean isQuiet() {
		return quiet;
	}

	public void setQuiet(boolean quiet) {
		this.quiet = quiet;
	}

	/**
	 * Jeden przebieg algorytmu genetycznego.
	 *
	 * @param generations Ilość pokoleń (iteracji) algorytmu.
	 * @return            Najlepszy znaleziony osobnik.
	 */
	public Specimen runEpoch(int generations) {
		List<Specimen> population = initialPopulation;
		computeFitness(population);
		Specimen best = initialPopulation.iterator().next();

		for (int i = 0; i < generations; i++) {
			population = nextGeneration(population);
			computeFitness(population);

			Specimen bestFromPopulation = Collections.max(population);
			best = Collections.max(Arrays.asList(best, bestFromPopulation));

			print(i, bestFromPopulation);
		}
		return best;
	}

	public List<Specimen> nextGeneration(List<Specimen> originalPopulation) {
		List<Specimen> newPopulation = new ArrayList<Specimen>(populationSize);
		newPopulation.addAll(getElite(originalPopulation));
		while (newPopulation.size() < originalPopulation.size()) {
			SelectionFunction selectionFunction = getSelectionFunction();
			Specimen parent1 = selectionFunction.select(originalPopulation);
			Specimen parent2 = selectionFunction.select(originalPopulation);
			List<Specimen> offspring;
			if (random.nextDouble() < getCrossoverProbability()) {
				offspring = getCrossoverOperator().produceOffspring(parent1, parent2);
			} else {
				offspring = Arrays.asList(parent1.createCopy(), parent2.createCopy());
			}
			for (Specimen specimen : offspring) {
				if (random.nextDouble() < getMutationProbability()) {
					getMutationOperator().mutate(specimen);
				}
				newPopulation.add(specimen);
				if (newPopulation.size() == originalPopulation.size()) {
					break;
				}
			}
		}
		return newPopulation;
	}

	private List<Specimen> getElite(List<Specimen> originalPopulation) {
		List<Specimen> elite = new ArrayList<Specimen>(eliteSize);
		if (eliteSize > 0) {
			List<Specimen> sorted = new ArrayList<Specimen>(originalPopulation);
			Collections.sort(sorted, Collections.reverseOrder());
			for (Specimen specimen : sorted.subList(0, eliteSize)) {
				elite.add(specimen.createCopy());
			}
		}
		return elite;
	}

	public void computeFitness(List<Specimen> population) {
		for (Specimen specimen : population) {
			specimen.computeFitness();
		}
	}

	private void print(int i, Specimen bestFromPopulation) {
		if (!quiet) {
			System.out.println((i + 1) + " " + bestFromPopulation.getFitness() +
					"\n" + bestFromPopulation.getPhenotype());
		}
	}
}
