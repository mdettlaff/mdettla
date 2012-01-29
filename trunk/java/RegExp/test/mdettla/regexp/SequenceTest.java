package mdettla.regexp;

import static mdettla.regexp.ExpressionAssertions.assertMatch;
import static mdettla.regexp.ExpressionAssertions.assertNoMatch;

import org.junit.Test;

public class SequenceTest {

	@Test
	public void test() {
		Sequence sequence = new Sequence(new Symbol('a'), new Symbol('b'));
		assertMatch(sequence, "ab", 2);
		assertNoMatch(sequence, "a", 1);
		assertNoMatch(sequence, "b", 0);
		assertNoMatch(sequence, "", 0);
	}
}
