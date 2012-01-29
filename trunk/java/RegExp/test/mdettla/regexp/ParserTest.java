package mdettla.regexp;

import static org.junit.Assert.assertEquals;

import org.junit.Before;
import org.junit.Test;

public class ParserTest {

	private Parser parser;

	@Before
	public void setUp() {
		parser = new Parser();
	}

	@Test
	public void testSymbol() {
		assertEquals(new Symbol('a'), parser.parse("a"));
	}

	@Test
	public void testAlternative() {
		assertEquals(new Alternative(new Symbol('a'), new Symbol('a')), parser.parse("a|a"));
		assertEquals(new Alternative(new Alternative(new Symbol('a'), new Symbol('a')), new Symbol('a')), parser.parse("a|a|a"));
	}

	@Test
	public void testSequence() {
		assertEquals(new Sequence(new Symbol('a'), new Symbol('a')), parser.parse("aa"));
		assertEquals(new Sequence(new Sequence(new Symbol('a'), new Symbol('a')), new Symbol('a')), parser.parse("aaa"));
	}

	@Test
	public void testAlternativeAndSequence() {
		assertEquals(new Alternative(new Symbol('a'), new Sequence(new Symbol('a'), new Symbol('a'))), parser.parse("a|aa"));
		assertEquals(new Alternative(new Sequence(new Symbol('a'), new Symbol('a')), new Symbol('a')), parser.parse("aa|a"));
	}

	@Test
	public void testParens() {
		assertEquals(new Alternative(new Symbol('a'), new Alternative(new Symbol('a'), new Symbol('a'))), parser.parse("a|(a|a)"));
		assertEquals(new Sequence(new Symbol('a'), new Sequence(new Symbol('a'), new Symbol('a'))), parser.parse("a(aa)"));
		assertEquals(new Sequence(new Alternative(new Symbol('a'), new Symbol('a')), new Symbol('a')), parser.parse("(a|a)a"));
		assertEquals(new Alternative(new Sequence(new Symbol('a'), new Symbol('a')), new Symbol('a')), parser.parse("(aa)|a"));
	}

	@Test(expected = ParseException.class)
	public void testParensError() {
		parser.parse("a(aa");
	}
}
