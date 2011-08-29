package mdettla.jga.operators.crossover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class MaximalPreservativeCrossoverTest {

	@Test
	public void testProduceOffspring() {
		// prepare
		Specimen parent1 = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		Specimen parent2 = new Sequence(Arrays.asList(2, 4, 6, 8, 7, 5, 3, 1));
		MaximalPreservativeCrossover crossover = new MaximalPreservativeCrossover();
		// test
		List<Specimen> offspring = crossover.produceOffspring(parent1, parent2, 2, 5);
		// verify
		assertNotNull(offspring);
		assertEquals(2, offspring.size());
		assertEquals(new Sequence(Arrays.asList(3, 4, 5, 2, 6, 8, 7, 1)), offspring.get(0));
		assertEquals(new Sequence(Arrays.asList(6, 8, 7, 1, 2, 3, 4, 5)), offspring.get(1));
	}
}
