package mdettla.jga.operators.mutation;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class DisplacementMutationTest {

	@Test
	public void testMutate() {
		// prepare
		Specimen specimen = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		DisplacementMutation mutation = new DisplacementMutation();
		// test
		mutation.mutate(specimen, 2, 5, 4);
		// verify
		assertEquals(new Sequence(Arrays.asList(1, 2, 6, 7, 3, 4, 5, 8)), specimen);
	}

	@Test
	public void testMutatePasteAtTheEnd() {
		// prepare
		Specimen specimen = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		DisplacementMutation mutation = new DisplacementMutation();
		// test
		mutation.mutate(specimen, 2, 5, 5);
		// verify
		assertEquals(new Sequence(Arrays.asList(1, 2, 6, 7, 8, 3, 4, 5)), specimen);
	}

	@Test
	public void testMutateZeroLengthCut() {
		// prepare
		Specimen specimen = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		DisplacementMutation mutation = new DisplacementMutation();
		// test
		mutation.mutate(specimen, 3, 3, 4);
		// verify
		assertEquals(new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8)), specimen);
	}
}
