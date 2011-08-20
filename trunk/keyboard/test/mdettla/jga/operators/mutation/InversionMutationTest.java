package mdettla.jga.operators.mutation;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import mdettla.jga.core.Sequence;
import mdettla.jga.core.Specimen;

import org.junit.Test;

public class InversionMutationTest {

	@Test
	public void testMutate() {
		// prepare
		Specimen specimen = new Sequence(Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8));
		InversionMutation mutation = new InversionMutation();
		// test
		mutation.mutate(specimen, 2, 5, 4);
		// verify
		assertEquals(new Sequence(Arrays.asList(1, 2, 6, 7, 5, 4, 3, 8)), specimen);
	}
}
