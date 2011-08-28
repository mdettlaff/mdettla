package mdettla.jga.operators.crossover;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class PositionBasedCrossover implements CrossoverOperator {

	private final Random random = new Random();

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		List<Integer> positions = getRandomPositions(parent1.getGenotypeLength());
		return produceOffspring(parent1, parent2, positions);
	}

	private List<Integer> getRandomPositions(int genotypeLength) {
		int positionsCount = random.nextInt(genotypeLength - 10) + 2;
		return Utils.randomSample(Utils.range(genotypeLength), positionsCount);
	}

	List<Specimen> produceOffspring(
			Specimen parent1, Specimen parent2, List<Integer> positions) {
		Specimen offspring1 = createOffspringSpecimen(parent1, parent2, positions);
		Specimen offspring2 = createOffspringSpecimen(parent2, parent1, positions);
		return Arrays.asList(offspring1, offspring2);
	}

	private Specimen createOffspringSpecimen(
			Specimen parent, Specimen otherParent, List<Integer> positions) {
		Specimen offspring = emptyOffspring(parent);
		Set<Object> offspringGenes = new HashSet<Object>();
		for (Integer position : positions) {
			Object gene = otherParent.getGeneAt(position);
			offspring.setGeneAt(position, gene);
			offspringGenes.add(gene);
		}
		int offspring1Index = 0;
		for (int i = 0; i < offspring.getGenotypeLength(); i++) {
			Object gene = parent.getGeneAt(i);
			if (offspringGenes.contains(gene)) {
				continue;
			}
			while (offspring.getGeneAt(offspring1Index) != null) {
				offspring1Index++;
			}
			offspring.setGeneAt(offspring1Index, gene);
			offspringGenes.add(gene);
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
