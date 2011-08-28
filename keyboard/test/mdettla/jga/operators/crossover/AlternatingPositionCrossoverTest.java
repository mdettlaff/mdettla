package mdettla.jga.operators.crossover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class AlternatingPositionCrossoverTest {

	@Test
	public void testProduceOffspring() {
		// prepare
		Specimen parent1 = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		Specimen parent2 = new Sequence(Arrays.asList(3, 7, 5, 1, 6, 8, 2, 4));
		CrossoverOperator crossover = new AlternatingPositionCrossover();
		// test
		List<Specimen> offspring = crossover.produceOffspring(parent1, parent2);
		// verify
		assertNotNull(offspring);
		assertEquals(2, offspring.size());
		assertEquals(new Sequence(Arrays.asList(1, 3, 2, 7, 5, 4, 6, 8)), offspring.get(0));
		assertEquals(new Sequence(Arrays.asList(3, 1, 7, 2, 5, 4, 6, 8)), offspring.get(1));
	}
}
