package mdettla.jga.operators.crossover;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import java.util.Arrays;
import java.util.List;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class PositionBasedCrossoverTest {

	@Test
	public void testProduceOffspring() {
		// prepare
		Specimen parent1 = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		Specimen parent2 = new Sequence(Arrays.asList(2, 4, 6, 8, 7, 5, 3, 1));
		List<Integer> positions = Arrays.asList(1, 2, 5);
		PositionBasedCrossover crossover = new PositionBasedCrossover();
		// test
		List<Specimen> offspring = crossover.produceOffspring(parent1, parent2, positions);
		// verify
		assertNotNull(offspring);
		assertEquals(2, offspring.size());
		assertEquals(new Sequence(Arrays.asList(1, 4, 6, 2, 3, 5, 7, 8)), offspring.get(0));
		assertEquals(new Sequence(Arrays.asList(4, 2, 3, 8, 7, 6, 5, 1)), offspring.get(1));
	}
}
