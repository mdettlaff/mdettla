package mdettla.keyboard.ga;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;

import org.junit.Test;

public class TextStatisticsTest {

	@Test
	public void test() throws IOException {
		String corpus = "foo bar baz";
		Reader corpusReader = new StringReader(corpus);
		TextStatistics stats = new TextStatistics(corpusReader);
		assertEquals(0, stats.getCharOccurences('x'));
		assertEquals(1, stats.getCharOccurences('f'));
		assertEquals(2, stats.getCharOccurences('o'));
		assertEquals(2, stats.getCharOccurences('b'));
		assertEquals(2, stats.getCharOccurences('a'));
		assertEquals(1, stats.getCharOccurences('r'));

		assertEquals(0, stats.getDiagraphOccurences(new Diagraph('a', 'b')));
		assertEquals(1, stats.getDiagraphOccurences(new Diagraph('f', 'o')));
		assertEquals(1, stats.getDiagraphOccurences(new Diagraph('a', 'r')));
		assertEquals(2, stats.getDiagraphOccurences(new Diagraph('b', 'a')));
	}
}