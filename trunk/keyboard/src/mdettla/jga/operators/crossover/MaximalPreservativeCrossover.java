package mdettla.jga.operators.crossover;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;

public class MaximalPreservativeCrossover implements CrossoverOperator {

	private final Random random = new Random();

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		int genotypeLength = parent1.getGenotypeLength();
		int[] cutPoints = {createCutPoint(genotypeLength), createCutPoint(genotypeLength)};
		Arrays.sort(cutPoints);
		return produceOffspring(parent1, parent2, cutPoints[0], cutPoints[1]);
	}

	private int createCutPoint(int genotypeLength) {
		return random.nextInt(genotypeLength + 1);
	}

	List<Specimen> produceOffspring(
			Specimen parent1, Specimen parent2, int cutPointLeft, int cutPointRight) {
		Specimen offspring1 = createOffspringSpecimen(
				parent1, parent2, cutPointLeft, cutPointRight);
		Specimen offspring2 = createOffspringSpecimen(
				parent2, parent1, cutPointLeft, cutPointRight);
		return Arrays.asList(offspring1, offspring2);
	}

	private Specimen createOffspringSpecimen(
			Specimen parent, Specimen otherParent, int cutPointLeft, int cutPointRight) {
		Specimen offspring = emptyOffspring(parent);
		Set<Object> offspringGenes = new HashSet<Object>();
		for (int i = cutPointLeft; i < cutPointRight; i++) {
			Object gene = parent.getGeneAt(i);
			offspring.setGeneAt(i - cutPointLeft, gene);
			offspringGenes.add(gene);
		}
		int offspringIndex = cutPointRight - cutPointLeft;
		for (int i = 0; i < otherParent.getGenotypeLength()
				&& offspringIndex < offspring.getGenotypeLength(); i++) {
			Object gene = otherParent.getGeneAt(i);
			if (!offspringGenes.contains(gene)) {
				offspring.setGeneAt(offspringIndex++, gene);
				offspringGenes.add(gene);
			}
		}
		return offspring;
	}

	private Specimen emptyOffspring(Specimen parent) {
		Specimen empty = parent.createCopy();
		for (int i = 0; i < empty.getGenotypeLength(); i++) {
			empty.setGeneAt(i, null);
		}
		return empty;
	}
}
