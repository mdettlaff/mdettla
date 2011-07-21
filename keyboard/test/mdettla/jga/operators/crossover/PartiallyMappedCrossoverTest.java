package mdettla.jga.operators.crossover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class PartiallyMappedCrossoverTest {

	@Test
	public void testProduceOffspring() {
		// prepare
		Specimen parent1 = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		Specimen parent2 = new Sequence(Arrays.asList(3, 7, 5, 1, 6, 8, 2, 4));
		PartiallyMappedCrossover crossover = new PartiallyMappedCrossover();
		// test
		List<Specimen> offspring = crossover.produceOffspring(parent1, parent2, 3, 6);
		// verify
		assertNotNull(offspring);
		assertEquals(2, offspring.size());
		assertEquals(new Sequence(Arrays.asList(4, 2, 3, 1, 6, 8, 7, 5)), offspring.get(0));
		assertEquals(new Sequence(Arrays.asList(3, 7, 8, 4, 5, 6, 2, 1)), offspring.get(1));
	}

	@Test
	public void testProduceOffspringWhenMappingSectionIsEmpty() {
		// prepare
		Specimen parent1 = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		Specimen parent2 = new Sequence(Arrays.asList(3, 7, 5, 1, 6, 8, 2, 4));
		PartiallyMappedCrossover crossover = new PartiallyMappedCrossover();
		// test
		List<Specimen> offspring = crossover.produceOffspring(parent1, parent2, 8, 8);
		// verify
		assertNotNull(offspring);
		assertEquals(2, offspring.size());
		assertEquals(parent1, offspring.get(0));
		assertEquals(parent2, offspring.get(1));
	}
}
