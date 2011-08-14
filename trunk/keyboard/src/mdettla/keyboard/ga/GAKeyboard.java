package mdettla.keyboard.ga;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import mdettla.jga.core.ConcurrentGeneticAlgorithm;
import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.Specimen;
import mdettla.jga.operators.crossover.CycleCrossover;
import mdettla.jga.operators.mutation.SwapMutation;

public class GAKeyboard {

	private static final int INITIAL_POPULATION_SIZE = 100;
	private static final int GENERATIONS_COUNT = 100;

	public static void main(String[] args) throws IOException {
		TextStatistics stats = getTextStatistics(args);
		List<Specimen> initialPopulation = getInitialPopulation(stats);

		GeneticAlgorithm ga = new ConcurrentGeneticAlgorithm(initialPopulation);
		ga.setMutationOperator(new SwapMutation());
		ga.setCrossoverOperator(new CycleCrossover());
		Specimen best = ga.runEpoch(GENERATIONS_COUNT);

		Specimen random = KeyboardLayout.createRandomInstance(stats);
		random.computeFitness();
		System.out.println("\nLosowy układ:\n" + random);
		System.out.println("\nQWERTY:\n" + KeyboardLayout.getQWERTYLayout(stats));
		System.out.println("\nDvorak:\n" + KeyboardLayout.getDvorakLayout(stats));
		System.out.println("\nNajlepiej przystosowany osobnik:\n" + best);
	}

	private static TextStatistics getTextStatistics(String[] texts) throws IOException {
		if (texts.length == 0) {
			texts = new String[] {"src/mdettla/keyboard/ga/resources/en/otoos11.txt"};
		}
		TextStatistics stats = new TextStatistics();
		for (String text : texts) {
			stats.read(new InputStreamReader(new FileInputStream(text)));
		}
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
