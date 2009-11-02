package mdettla.jga.operators.crossover;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class CutPointCrossover implements CrossoverOperator {

	private static Random random = new Random();

	private int cutPointCount;

	public CutPointCrossover(int cutPointCount) {
		if (cutPointCount < 1) {
			throw new IllegalArgumentException(
					"Ilość punktów przecięcia musi być większa od 0.");
		}
		this.cutPointCount = cutPointCount;
	}

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		int genotypeLen = parent1.getGenotypeLength();
		Specimen[] parents = new Specimen[] {parent1, parent2};

		Collection<Integer> cutPoints = Utils.randomSample(
				Utils.range(1, genotypeLen), cutPointCount);
		int currentParent = random.nextInt(2);
		Specimen offspring = parent1.createCopy();
		for (int i = 0; i < genotypeLen; i++) {
			if (cutPoints.contains(i)) {
				currentParent = 1 - currentParent;
			}
			offspring.setGeneAt(i, parents[currentParent].getGeneAt(i));
		}
		return Arrays.asList(offspring);
	}
}
