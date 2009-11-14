package mdettla.jga.operators.mutation;

import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

/**
 * Mutacja jednopunktowa. W wyniku mutacji zostaje zmieniony zawsze dokładnie
 * jeden gen.
 */
public class OnePointMutation implements MutationOperator {

	private static Random random = new Random();

	@Override
	public void mutate(Specimen specimen) {
		int randomIndex = random.nextInt(specimen.getGenotypeLength());
		specimen.setOppositeGeneValueAt(randomIndex);
	}
}
