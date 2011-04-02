package mdettla.jga.operators.crossover;

import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class CutPointCrossoverTest {

	@Test
	public void testCutPointCrossover() {
		CrossoverOperator onePointCrossover = new CutPointCrossover(1);
		Specimen p1 = new Sequence(Arrays.asList(0, 1, 2, 3, 4));
		Specimen p2 = new Sequence(Arrays.asList(5, 6, 7, 8, 9));
		Specimen offspring = onePointCrossover.produceOffspring(p1, p2).get(0);
		List<Specimen> possibleOffspring = new ArrayList<Specimen>();
		possibleOffspring.add(new Sequence(Arrays.asList(0, 6, 7, 8, 9)));
		possibleOffspring.add(new Sequence(Arrays.asList(0, 1, 7, 8, 9)));
		possibleOffspring.add(new Sequence(Arrays.asList(0, 1, 2, 8, 9)));
		possibleOffspring.add(new Sequence(Arrays.asList(0, 1, 2, 3, 9)));
		possibleOffspring.add(new Sequence(Arrays.asList(5, 6, 7, 8, 4)));
		possibleOffspring.add(new Sequence(Arrays.asList(5, 6, 7, 3, 4)));
		possibleOffspring.add(new Sequence(Arrays.asList(5, 6, 2, 3, 4)));
		possibleOffspring.add(new Sequence(Arrays.asList(5, 1, 2, 3, 4)));
		assertTrue(possibleOffspring.contains(offspring));
	}
}
