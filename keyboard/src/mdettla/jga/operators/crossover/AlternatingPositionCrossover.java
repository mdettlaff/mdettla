package mdettla.jga.operators.crossover;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;

public class AlternatingPositionCrossover implements CrossoverOperator {

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		Specimen offspring1 = createOffspringSpecimen(parent1, parent2);
		Specimen offspring2 = createOffspringSpecimen(parent2, parent1);
		return Arrays.asList(offspring1, offspring2);
	}

	private Specimen createOffspringSpecimen(Specimen parent, Specimen otherParent) {
		Specimen offspring = emptyOffspring(parent);
		Set<Object> offspringGenes = new HashSet<Object>();
		int offspringIndex = 0;
		for (int i = 0; i < offspring.getGenotypeLength() * 2
				&& offspringIndex < offspring.getGenotypeLength(); i++) {
			Specimen currentParent = i % 2 == 0 ? parent : otherParent;
			int index = i / 2;
			Object gene = currentParent.getGeneAt(index);
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
