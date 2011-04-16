package mdettla.keyboard.ga;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.ArrayList;
import java.util.List;

import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.Specimen;
import mdettla.jga.operators.crossover.CycleCrossover;
import mdettla.jga.operators.mutation.SwapMutation;

public class GAKeyboard {

	private static final int INITIAL_POPULATION_SIZE = 100;
	private static final int GENERATIONS_COUNT = 50;

	public static void main(String[] args) throws IOException {
		TextStatistics stats = getTextStatistics();
		List<Specimen> initialPopulation = getInitialPopulation(stats);

		GeneticAlgorithm ga = new GeneticAlgorithm(initialPopulation);
		ga.setMutationOperator(new SwapMutation());
		ga.setCrossoverOperator(new CycleCrossover());
		Specimen best = ga.runEpoch(GENERATIONS_COUNT);

		System.out.println("\nLosowy układ:\n" +
				KeyboardLayout.createRandomInstance(stats));
		System.out.println("\nQWERTY:\n" + KeyboardLayout.getQWERTYLayout(stats));
		System.out.println("\nDvorak:\n" + KeyboardLayout.getDvorakLayout(stats));
		System.out.println("\nNajlepiej przystosowany osobnik:\n" + best);
	}

	private static TextStatistics getTextStatistics() throws IOException {
		Reader corpusReader = new InputStreamReader(
				GAKeyboard.class.getResourceAsStream("resources/otoos11.txt"));
		TextStatistics stats = new TextStatistics(corpusReader);
		System.out.println("Długość analizowanego tekstu: "
				+ stats.getTextLength() + " znaków");
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
