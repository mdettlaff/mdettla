import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
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

	private static final int ITERATIONS = 30;
	private static final double DEFAULT_CROSSOVER_PROBABILITY = .6;

	private static int verticesCount;

	public static void main(String[] args) throws IOException {
		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		Graph<Integer> graph = Graph.readGraph(in);
		IndependentSet.setGraph(
				new ArrayList<Integer>(graph.getVertices()), graph.getEdges());
		verticesCount = graph.getVertices().size();

		System.out.println("Algorytm genetyczny dla wyznaczania " +
				"maksymalnego zbioru niezależnego.");
		System.out.println("\nDomyślne parametry algorytmu genetycznego:");
		System.out.println("Selekcja turniejowa o rozmiarze turnieju 4");
		System.out.println("Mutacja 1-punktowa z prawdopodobieństwem 1/n");
		System.out.println("Krzyżowanie 1-punktowe z prawdopodobieństwem " +
				DEFAULT_CROSSOVER_PROBABILITY);
		System.out.println("Rozmiar populacji µ/4 = " + verticesCount / 4);

		runExperiment("\nEksperyment 1, domyślne parametry:",
				null, null, null, null);
		runExperiment("\nEksperyment 2, µ = n/2 = " + verticesCount / 2 + ":",
				verticesCount / 2, null, null, null);
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
		// ustawiamy parametry algorytmu genetycznego
		if (populationSize == null) {
			populationSize = verticesCount / 4;
		}
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < populationSize; i++) {
			initialPopulation.add(IndependentSet.createRandomInstance());
		}
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
			ga.setSelectionFunction(new TournamentSelection(4));
		}
		//ga.setPrintResults(true);
		// uruchamiamy algorytm genetyczny
		long timeBegin = System.currentTimeMillis();
		Specimen best = ga.runEpoch(ITERATIONS);
		long timeEnd = System.currentTimeMillis();
		System.out.println("Najlepiej przystosowany osobnik " +
				"(wartość przystosowania = " + best.getFitness() + "):");
		System.out.println("Rozmiar zbioru niezależnego = " +
				((IndependentSet)best).getContent().size());
		// liczymy średnie przystosowanie
		List<Specimen> lastPopulation = ga.getLastPopulation();
		double sumFitness = 0;
		for (Specimen specimen : lastPopulation) {
			sumFitness += specimen.getFitness().doubleValue();
		}
		double meanFitness = sumFitness / lastPopulation.size();
		System.out.println(String.format("Średnie przystosowanie: %.2f",
					meanFitness));
		// liczymy odchylenie standardowe
		double[] fitnessArray = new double[lastPopulation.size()];
		for (int i = 0; i < lastPopulation.size(); i++) {
			fitnessArray[i] = lastPopulation.get(i).getFitness().doubleValue();
		}
		double standardDeviation =
			new StandardDeviation().evaluate(fitnessArray, meanFitness);
		System.out.println(String.format("Odchylenie standardowe: %.2f",
					standardDeviation));
		// czas trwania eksperymentu
		System.out.println("Czas wykonania: " + (timeEnd - timeBegin) + " ms");
	}
}
