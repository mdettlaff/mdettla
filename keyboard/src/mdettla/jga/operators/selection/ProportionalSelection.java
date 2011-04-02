package mdettla.jga.operators.selection;

import java.util.List;
import java.util.Random;

import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;

/**
 * Selekcja proporcjonalna, wykorzystująca regułę ruletki. Rodzice wybierani
 * są z prawdopodobieństwem proporcjonalnym do ich dostosowania. Jeżeli
 * dostosowanie i-tego osobnika oznaczymy przez <code>f_i</code>, to
 * prawdopodobieństwo jego selekcji jest równe <code>p_i = f_i/F</code>, gdzie
 * <code>F</code> oznacza sumę dostosowań wszystkich osobników populacji.
 * Oczywiście taki sposób określania prawdopodobieństwa jest poprawny tylko
 * wówczas, jeżeli dostosowanie każdego osobnika jest liczbą nieujemną,
 * a przynajmniej jednego osobnika - liczbą dodatnią.
 */
public class ProportionalSelection implements SelectionFunction {

	private static Random random = new Random();

	@Override
	public Specimen select(List<Specimen> population) {
		double totalFitness = 0;
		for (Specimen s : population) {
			totalFitness += s.getFitness().doubleValue();
		}
		double r = random.nextDouble() * totalFitness;
		double sumFitness = 0;
		for (Specimen specimen : population) {
			sumFitness += specimen.getFitness().doubleValue();
			if (sumFitness > r) {
				return specimen;
			}
		}
		throw new AssertionError(); // nie powinno dojść do tego miejsca
	}
}
