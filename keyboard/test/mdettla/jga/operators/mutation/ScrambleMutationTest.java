package mdettla.jga.operators.mutation;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class ScrambleMutationTest {

	@Test
	public void testMutate() {
		// prepare
		Specimen specimen = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		ScrambleMutation mutation = new ScrambleMutation();
		// test
		mutation.mutate(specimen, 2, 5);
		// verify
		assertEquals(1, specimen.getGeneAt(0));
		assertEquals(2, specimen.getGeneAt(1));
		assertEquals(6, specimen.getGeneAt(5));
		assertEquals(7, specimen.getGeneAt(6));
		assertEquals(8, specimen.getGeneAt(7));
	}
}
