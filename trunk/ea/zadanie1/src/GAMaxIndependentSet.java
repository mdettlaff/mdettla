import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;
import mdettla.jga.operators.crossover.CutPointCrossover;
import mdettla.jga.operators.selection.RankSelection;
import mdettla.jga.operators.selection.TournamentSelection;

import org.apache.commons.math.stat.descriptive.moment.StandardDeviation;

/**
 * Znajdowanie największego zbioru niezależnego w grafie za pomocą
 * algorytmu genetycznego.
 *
 * Graf należy podać ze standardowego wejścia w formacie DIMACS Graph.
 */
public class GAMaxIndependentSet {

	private static final int EPOCH_REPEATS = 30;
	private static final int ITERATIONS = 50;
	private static final double DEFAULT_CROSSOVER_PROBABILITY = .6;

	private static int verticesCount;
	private static int edgesCount;

	public static void main(String[] args) throws IOException {
		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		Graph<Integer> graph = Graph.readGraph(in);
		IndependentSet.setGraph(
				new ArrayList<Integer>(graph.getVertices()), graph.getEdges());
		verticesCount = graph.getVertices().size();
		edgesCount = graph.getEdges().size();

		System.out.println("Algorytm genetyczny dla wyznaczania " +
				"maksymalnego zbioru niezależnego.");
		System.out.println("\nDomyślne parametry algorytmu genetycznego:");
		System.out.println("Selekcja turniejowa o rozmiarze turnieju 4");
		System.out.println("Mutacja 1-punktowa z prawdopodobieństwem 1/n");
		System.out.println("Krzyżowanie 1-punktowe z prawdopodobieństwem " +
				DEFAULT_CROSSOVER_PROBABILITY);
		System.out.println("Rozmiar populacji µ/4 = " + edgesCount / 4);
		System.out.println("Liczba iteracji: " + ITERATIONS);
		System.out.println("Wartości są uśredniane po " + EPOCH_REPEATS +
				" powtórzeniach");

		runExperiment("\nEksperyment 1, domyślne parametry:",
				null, null, null, null);
		runExperiment("\nEksperyment 2, µ = n/2 = " + edgesCount / 2 + ":",
				edgesCount / 2, null, null, null);
		runExperiment("\nEksperyment 3, krzyżowanie 2-punktowe:",
				null, null, new CutPointCrossover(2), null);
		runExperiment("\nEksperyment 4, brak krzyżowania:",
				null, .0, null, null);
		runExperiment("\nEksperyment 5, selekcja rangowa:",
				null, null, null, new RankSelection());
	}

	private static void runExperiment(String name, Integer populationSize,
			Double p_c, CrossoverOperator crossover,
			SelectionFunction selection) {
		System.out.println(name);
		// populacja początkowa
		if (populationSize == null) {
			populationSize = edgesCount / 4;
		}
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < populationSize; i++) {
			initialPopulation.add(IndependentSet.createRandomInstance());
		}
		// ustawiamy parametry algorytmu genetycznego
		GeneticAlgorithm ga = new GeneticAlgorithm(initialPopulation);
		if (crossover != null) {
			ga.setCrossoverOperator(crossover);
		} else {
			ga.setCrossoverOperator(new CutPointCrossover(1));
		}
		if (p_c != null) {
			ga.setCrossoverProbability(p_c);
		} else {
			ga.setCrossoverProbability(DEFAULT_CROSSOVER_PROBABILITY);
		}
		ga.setMutationProbability(1);
		if (selection != null) {
			ga.setSelectionFunction(selection);
		} else {
			ga.setSelectionFunction(new TournamentSelection(3));
		}
		// uruchamiamy algorytm genetyczny
		long timeBegin = System.currentTimeMillis();
		List<Double> means = new ArrayList<Double>();
		List<List<Specimen>> bestInItersEpochs =
			new ArrayList<List<Specimen>>();
		for (int i = 0; i < ITERATIONS; i++) {
			bestInItersEpochs.add(new ArrayList<Specimen>());
		}
		for (int i = 0; i < EPOCH_REPEATS; i++) {
			Iterator<List<Specimen>> it = ga.iterator();
			List<Specimen> generation = null;
			for (int j = 0; j < ITERATIONS; j++) {
				generation = it.next();
				bestInItersEpochs.get(j).add(Collections.max(generation));
			}
			means.add(meanFitness(generation)); // ostatnie pokolenie
		}
		long timeEnd = System.currentTimeMillis();
		// ewolucja najlepszego rozwiązania, po uśrednieniu
		System.out.println("Ewolucja najlepszego rozwiązania:");
		System.out.println("Iter.\tPrzystosowanie najlepszego w populacji");
		for (int i = 0; i < ITERATIONS; i++) {
			System.out.println(String.format("%d\t%.2f",
						i + 1, meanFitness(bestInItersEpochs.get(i))));
		}
		// liczymy średnie przystosowanie
		double totalMeanFitness = mean(means);
		System.out.println(String.format("Średnie przystosowanie: %.2f",
					totalMeanFitness));
		// liczymy odchylenie standardowe
		System.out.println(String.format("Odchylenie standardowe: %.2f",
					standardDeviation(means, totalMeanFitness)));
		// czas trwania eksperymentu
		System.out.println("Czas wykonania: " + (timeEnd - timeBegin) + " ms");
	}

	private static double mean(List<? extends Number> values) {
		double sum = 0;
		for (Number value : values) {
			sum += value.doubleValue();
		}
		return sum / values.size();
	}

	private static double meanFitness(List<Specimen> population) {
		List<Number> fitness = new ArrayList<Number>();
		for (Specimen specimen : population) {
			fitness.add(specimen.getFitness().doubleValue());
		}
		return mean(fitness);
	}

	private static double standardDeviation(List<? extends Number> values,
			double mean) {
		double[] arr = new double[values.size()];
		for (int i = 0; i < values.size(); i++) {
			arr[i] = values.get(i).doubleValue();
		}
		return new StandardDeviation().evaluate(arr, mean);
	}
}
