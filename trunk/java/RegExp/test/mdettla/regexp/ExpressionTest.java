package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;

import org.junit.Test;

public class ExpressionTest {

	@Test
	public void test() {
		Star star = new Star(new Sequence(new Symbol('a'), new Symbol('b')));
		Sequence sequence = new Sequence(new Symbol('a'), new Symbol('c'));
		Alternative alternative = new Alternative(star, sequence);
		assertMatch(alternative, "ac", 1);
	}
}
