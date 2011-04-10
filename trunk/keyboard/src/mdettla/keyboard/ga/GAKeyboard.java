package mdettla.keyboard.ga;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.Specimen;
import mdettla.jga.operators.crossover.CycleCrossover;
import mdettla.jga.operators.mutation.SwapMutation;

public class GAKeyboard {

	private static final int INITIAL_POPULATION_SIZE = 100;
	private static final int GENERATIONS_COUNT = 75;

	public static void main(String[] args) throws IOException {
		TextStatistics stats = getTextStatistics();
		List<Specimen> initialPopulation = getInitialPopulation(stats);

		GeneticAlgorithm ga = new GeneticAlgorithm(initialPopulation);
		ga.setMutationOperator(new SwapMutation());
		ga.setCrossoverOperator(new CycleCrossover());
		Specimen best = ga.runEpoch(GENERATIONS_COUNT);

		System.out.println("Najlepiej przystosowany osobnik:\n" + best);
	}

	private static TextStatistics getTextStatistics() throws IOException {
		String corpus = "foo bar baz bbb";
		Reader corpusReader = new StringReader(corpus);
		TextStatistics stats = new TextStatistics(corpusReader);
		return stats;
	}

	private static List<Specimen> getInitialPopulation(TextStatistics stats) {
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < INITIAL_POPULATION_SIZE; i++) {
			initialPopulation.add(KeyboardLayout.createRandomInstance(stats));
		}
		return initialPopulation;
	}
}
