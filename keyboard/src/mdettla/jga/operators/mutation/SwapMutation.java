package mdettla.jga.operators.mutation;

import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

/**
 * Mutacja polegająca na zamianie dwóch genów miejscami.
 */
public class SwapMutation implements MutationOperator {

	private Random random = new Random();

	@Override
	public void mutate(Specimen specimen) {
		int position1 = random.nextInt(specimen.getGenotypeLength());
		int position2 = random.nextInt(specimen.getGenotypeLength());
		Object tmp = specimen.getGeneAt(position1);
		specimen.setGeneAt(position1, specimen.getGeneAt(position2));
		specimen.setGeneAt(position2, tmp);
	}
}
