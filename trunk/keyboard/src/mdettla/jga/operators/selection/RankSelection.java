package mdettla.jga.operators.selection;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;

/**
 * Selekcja rangowa.
 * Osobnikowi o najmniejszej wartości przystosowania przypisujemy rangę 1,
 * kolejnemu 2 i tak dalej (najlepiej przystosowany otrzymuje rangę równą
 * liczebności populacji). Prawdopodobieństwo selekcji osobnika jest
 * proporcjonalne do jego rangi.
 */
public class RankSelection implements SelectionFunction {

	private static Random random = new Random();

	@Override
	public Specimen select(List<Specimen> population) {
		List<Specimen> sortedPopulation = new ArrayList<Specimen>(population);
		Collections.sort(sortedPopulation);
		double totalRank = 0;
		for (int i = 1; i <= population.size(); i++) {
			totalRank += i;
		}
		double r = random.nextDouble() * totalRank;
		double sumRank = 0;
		for (int i = 0; i < sortedPopulation.size(); i++) {
			sumRank += i + 1;
			if (sumRank > r) {
				return sortedPopulation.get(i);
			}
		}
		throw new AssertionError(); // nie powinno dojść do tego miejsca
	}
}
