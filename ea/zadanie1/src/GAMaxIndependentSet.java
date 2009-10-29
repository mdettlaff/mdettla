import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.JGAException;
import mdettla.jga.core.Specimen;

public class GAMaxIndependentSet {

	public static void main(String[] args) throws JGAException {
		System.out.println("Algorytm genetyczny dla wyznaczania " +
				"maksymalnego zbioru niezależnego.");

		/* Ustawiamy graf, na którym definiujemy zbiór niezależny.
		 * Graf ten wygląda tak:
		 *
		 * 1-2-5-6
		 * | |
		 * 3-4
		 */
		List<Integer> vertices = Arrays.asList(1, 2, 3, 4, 5, 6);
		Set<Edge<Integer>> edges = new HashSet<Edge<Integer>>();
		edges.add(new Edge<Integer>(1, 2));
		edges.add(new Edge<Integer>(1, 3));
		edges.add(new Edge<Integer>(3, 4));
		edges.add(new Edge<Integer>(2, 4));
		edges.add(new Edge<Integer>(2, 5));
		edges.add(new Edge<Integer>(5, 6));
		IndependentSet.setGraph(vertices, edges);

		final int POPULATION_SIZE = 5;
		// inicjujemy populację początkową
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < POPULATION_SIZE; i++) {
			initialPopulation.add(IndependentSet.createRandomInstance());
		}

		// uruchamiamy algorytm genetyczny
		GeneticAlgorithm ga = new GeneticAlgorithm(initialPopulation);
		Specimen best = ga.runEpoch(30);

		System.out.println("Najlepiej przystosowany osobnik " +
				"(wartość przystosowania = " + best.getFitness() + "):");
		System.out.println(((IndependentSet)best).getContent());
	}
}
