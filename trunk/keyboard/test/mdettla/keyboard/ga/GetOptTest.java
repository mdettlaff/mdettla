package mdettla.keyboard.ga;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Arrays;

import org.junit.Test;

public class GetOptTest {

	@Test
	public void testGetOpt() {
		String[] args = {"foo=bar", "baz=quux", "file1", "file2"};
		GetOpt getOpt = new GetOpt(args);
		assertTrue(getOpt.isOptionSet("foo"));
		assertFalse(getOpt.isOptionSet("foobar"));
		assertEquals("bar", getOpt.getValue("foo"));
		assertEquals("quux", getOpt.getValue("baz"));
		assertEquals(Arrays.asList("file1", "file2"), getOpt.getArguments());
	}
}
