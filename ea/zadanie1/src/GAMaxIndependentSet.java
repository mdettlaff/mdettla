import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.JGAException;
import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;
import mdettla.jga.operators.crossover.CutPointCrossover;
import mdettla.jga.operators.selection.RankSelection;
import mdettla.jga.operators.selection.TournamentSelection;

import org.apache.commons.math.stat.descriptive.moment.StandardDeviation;

/**
 * Znajdowanie największego zbioru niezależnego w grafie za pomocą
 * algorytmu genetycznego.
 */
public class GAMaxIndependentSet {

	private static final int EDGES_COUNT = 36;
	private static final double DEFAULT_CROSSOVER_PROBABILITY = .6;

	public static void main(String[] args) throws JGAException {
		initGraph();

		System.out.println("Algorytm genetyczny dla wyznaczania " +
				"maksymalnego zbioru niezależnego.");
		System.out.println("\nDomyślne parametry algorytmu genetycznego:");
		System.out.println("Selekcja turniejowa o rozmiarze turnieju 4");
		System.out.println("Mutacja 1-punktowa z prawdopodobieństwem 1/n");
		System.out.println("Krzyżowanie 1-punktowe z prawdopodobieństwem " +
				DEFAULT_CROSSOVER_PROBABILITY);
		System.out.println("Rozmiar populacji µ/4 = " + EDGES_COUNT / 4);

		runExperiment("\nEksperyment 1, domyślne parametry:",
				null, null, null, null);
		runExperiment("\nEksperyment 2, µ = n/2 = " + EDGES_COUNT / 2 + ":",
				EDGES_COUNT / 2, null, null, null);
		runExperiment("\nEksperyment 3, krzyżowanie 2-punktowe:",
				null, null, new CutPointCrossover(2), null);
		runExperiment("\nEksperyment 4, brak krzyżowania:",
				null, .0, null, null);
		runExperiment("\nEksperyment 4, selekcja rangowa:",
				null, null, null, new RankSelection());
	}

	/**
	 * Tworzy graf, na którym definiujemy zbiór niezależny.
	 */
	private static void initGraph() {
		List<Integer> vertices = Utils.range(1, 25); // 1 do 24 włącznie
		Set<Edge<Integer>> edges = new HashSet<Edge<Integer>>();
		edges.add(new Edge<Integer>(1, 2));
		edges.add(new Edge<Integer>(2, 3));
		edges.add(new Edge<Integer>(3, 4));
		edges.add(new Edge<Integer>(4, 5));
		edges.add(new Edge<Integer>(5, 6));
		edges.add(new Edge<Integer>(6, 7));
		edges.add(new Edge<Integer>(7, 8));
		edges.add(new Edge<Integer>(8, 9));
		edges.add(new Edge<Integer>(9, 10));
		edges.add(new Edge<Integer>(10, 11));
		edges.add(new Edge<Integer>(11, 12));
		edges.add(new Edge<Integer>(12, 1));
		edges.add(new Edge<Integer>(1, 13));
		edges.add(new Edge<Integer>(2, 14));
		edges.add(new Edge<Integer>(3, 15));
		edges.add(new Edge<Integer>(4, 16));
		edges.add(new Edge<Integer>(5, 17));
		edges.add(new Edge<Integer>(6, 18));
		edges.add(new Edge<Integer>(7, 19));
		edges.add(new Edge<Integer>(8, 20));
		edges.add(new Edge<Integer>(9, 21));
		edges.add(new Edge<Integer>(10, 22));
		edges.add(new Edge<Integer>(11, 23));
		edges.add(new Edge<Integer>(12, 24));
		edges.add(new Edge<Integer>(13, 17));
		edges.add(new Edge<Integer>(14, 18));
		edges.add(new Edge<Integer>(15, 19));
		edges.add(new Edge<Integer>(16, 20));
		edges.add(new Edge<Integer>(17, 21));
		edges.add(new Edge<Integer>(18, 22));
		edges.add(new Edge<Integer>(19, 23));
		edges.add(new Edge<Integer>(20, 24));
		edges.add(new Edge<Integer>(21, 13));
		edges.add(new Edge<Integer>(22, 14));
		edges.add(new Edge<Integer>(23, 15));
		edges.add(new Edge<Integer>(24, 16));
		IndependentSet.setGraph(vertices, edges);
	}

	private static void runExperiment(String name, Integer populationSize,
			Double p_c, CrossoverOperator crossover,
			SelectionFunction selection) throws JGAException {
		System.out.println(name);
		// ustawiamy parametry algorytmu genetycznego
		if (populationSize == null) {
			populationSize = EDGES_COUNT / 4;
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
		if (selection != null) {
			ga.setSelectionFunction(selection);
		} else {
			ga.setSelectionFunction(new TournamentSelection(4));
		}
		//ga.setPrintResults(true);
		// uruchamiamy algorytm genetyczny
		Specimen best = ga.runEpoch(30);
		System.out.println("Najlepiej przystosowany osobnik " +
				"(wartość przystosowania = " + best.getFitness() + "):");
		System.out.println(((IndependentSet)best).getContent());
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
	}
}
