package mdettla.jga.operators.mutation;

import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

public class InsertionMutation implements MutationOperator {

	private final Random random = new Random();

	@Override
	public void mutate(Specimen specimen) {
		int position1 = random.nextInt(specimen.getGenotypeLength());
		int position2 = random.nextInt(specimen.getGenotypeLength());
		mutate(specimen, position1, position2);
	}

	void mutate(Specimen specimen, int position1, int position2) {
		Object tmp = specimen.getGeneAt(position1);
		for (int i = position1; i < position2; i++) {
			specimen.setGeneAt(i, specimen.getGeneAt(i + 1));
		}
		specimen.setGeneAt(position2, tmp);
	}
}
