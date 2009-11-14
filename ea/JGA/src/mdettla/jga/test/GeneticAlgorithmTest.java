package mdettla.jga.test;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.List;

import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class GeneticAlgorithmTest {

	@Test
	public void testRunEpoch() {
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < 100; i++) {
			initialPopulation.add(Text.createRandomInstance());
		}

		GeneticAlgorithm ga = new GeneticAlgorithm(initialPopulation);
		Specimen best = ga.runEpoch(200);

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
