package mdettla.jga.operators.mutation;

import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

/**
 * Mutacja probabilistyczna. Każdy gen ma jednakowe prawdopodobieństwo mutacji
 * równe {@code 1/n}, gdzie {@code n} to długość genotypu. Oznacza to,
 * że mutacji może ulec jeden lub więcej genów (albo żaden).
 */
public class ProbabilisticMutation implements MutationOperator {

	private static Random random = new Random();

	@Override
	public void mutate(Specimen specimen) {
		// prawdopodobieństwo mutacji dla pojedynczego genu
		double p = 1.0 / specimen.getGenotypeLength();
		for (int i = 0; i < specimen.getGenotypeLength(); i++) {
			if (random.nextDouble() < p) {
				specimen.setOppositeGeneValueAt(i);
			}
		}
	}
}
