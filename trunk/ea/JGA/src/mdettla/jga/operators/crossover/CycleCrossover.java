package mdettla.jga.operators.crossover;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class CycleCrossover implements CrossoverOperator {

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		Random random = new Random();
		Specimen[] parents = new Specimen[] { parent1, parent2 };
		Specimen offspring;

		offspring = parent1.createCopy();
		// geny, których jeszcze nie skopiowaliśmy do genotypu potomka
		List<Object> unused =
			new ArrayList<Object>(offspring.getGenotypeLength());
		for (int i = 0; i < parent1.getGenotypeLength(); i++) {
			unused.add(parent1.getGeneAt(i));
		}
		List<Integer> randomOrder = Utils.range(offspring.getGenotypeLength());
		for (Integer i : randomOrder) {
			int parentIndex = random.nextInt(2);
			if (unused.contains(parents[parentIndex].getGeneAt(i))) {
				Object gene = parents[parentIndex].getGeneAt(i);
				offspring.setGeneAt(i, gene);
				unused.remove(gene);
			} else if (unused.contains(parents[1 - parentIndex].getGeneAt(i))) {
				Object gene = parents[1 - parentIndex].getGeneAt(i);
				offspring.setGeneAt(i, gene);
				unused.remove(gene);
			} else {
				// bierzemy arbitralnie wybrany niepowtarzający się gen
				Object gene = unused.iterator().next();
				offspring.setGeneAt(i, gene);
				unused.remove(gene);
			}
		}
		return Arrays.asList(offspring);
	}
}
