package mdettla.jga.core;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

public class GeneticAlgorithmIntegrationTest {

	private static final int ITERATIONS = 100;

	private List<Specimen> initialPopulation;
	private GeneticAlgorithm ga;

	@Before
	public void setUp() throws Exception {
		initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < 100; i++) {
			initialPopulation.add(Text.createRandomInstance());
		}
		ga = new ConcurrentGeneticAlgorithm(initialPopulation);
	}

	/**
	 * Wykonanie algorytmu poprzez {@code runEpoch}.
	 */
	@Test
	public void testRunEpoch() {
		System.out.println("testRunEpoch");
		Specimen best = ga.runEpoch(ITERATIONS);
		checkResults(best);
	}

	private void checkResults(Specimen best) {
		System.out.println("Najlepiej przystosowany osobnik " +
				"(wartość przystosowania = " + best.getFitness() + "):");
		System.out.println(((Text)best).getPhenotype());
		int nonMatchingGenes = 0;
		for (int i = 0; i < Text.TARGET.length(); i++) {
			if (Text.TARGET.charAt(i) != (Character)best.getGeneAt(i)) {
				nonMatchingGenes++;
			}
		}
		assertTrue("Osobnik docelowy za bardzo różni się od uzyskanego " +
				"przez algorytm genetyczny.",
				nonMatchingGenes <= 2);
	}
}
