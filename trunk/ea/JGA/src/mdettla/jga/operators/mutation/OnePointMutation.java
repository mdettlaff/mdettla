package mdettla.jga.operators.mutation;

import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

public class OnePointMutation implements MutationOperator {

	@Override
	public void mutate(Specimen specimen) {
		Random random = new Random();
		int randomIndex = random.nextInt(specimen.getGenotypeLength());
		specimen.setOppositeGeneValueAt(randomIndex);
	}
}
