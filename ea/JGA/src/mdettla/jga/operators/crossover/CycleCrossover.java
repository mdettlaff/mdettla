package mdettla.jga.operators.crossover;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Gene;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class CycleCrossover implements CrossoverOperator {

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		Random random = new Random();
		Specimen[] parents = new Specimen[] { parent1, parent2 };
		Specimen offspring;

		offspring = parent1.createInstance();
		// geny, których jeszcze nie skopiowaliśmy do genotypu potomka
		Set<Gene> unused  = new HashSet<Gene>();
		for (int i = 0; i < parent1.getGenotypeLength(); i++) {
			unused.add(parent1.getGeneAt(i));
		}
		List<Integer> randomOrder = Utils.range(offspring.getGenotypeLength());
		for (Integer i : randomOrder) {
			int parentIndex = random.nextInt(2);
			if (unused.contains(parents[parentIndex].getGeneAt(i))) {
				Gene gene = parents[parentIndex].getGeneAt(i);
				offspring.setGeneAt(i, gene);
				unused.remove(gene);
			} else if (unused.contains(parents[1 - parentIndex].getGeneAt(i))) {
				Gene gene = parents[1 - parentIndex].getGeneAt(i);
				offspring.setGeneAt(i, gene);
				unused.remove(gene);
			} else {
				// bierzemy arbitralnie wybrany niepowtarzający się gen
				offspring.setGeneAt(i, unused.iterator().next());
			}
		}
		return Arrays.asList(offspring);
	}
}
