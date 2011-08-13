package mdettla.keyboard.ga;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

public class KeyboardLayoutTest {

	private static final String CORPUS =
		"Jestem przekonany, że wszystkie te dobre uczynki mam do " +
		"zawdzięczenia mojej taktyce niestosowania przemocy. Gdybym " +
		"zabiegał o ponowne przyjęcie mnie do kasty, usiłował ją rozbić " +
		"na poszczególne obozy lub zachował się prowokacyjnie wobec jej " +
		"członków, ci z pewnością odwzajemniliby się w podobny sposób i " +
		"zamiast szczęśliwie uniknąć burzy po przybyciu z Anglii, wpadłbym " +
		"w sam wir rozagitowania, kto wie, czy nie uprawianego częściowo " +
		"za moimi plecami. Mój stosunek do żony wciąż jeszcze nie był taki, " +
		"jakiego pragnąłem. Nawet pobyt w Anglii nie uleczył mnie " +
		"z zazdrości. Byłem w dalszym ciągu drobnostkowy i zazdrosny o byle " +
		"co, a wszystkie moje najbardziej utajone pragnienia zostawały nie " +
		"zaspokojone. Fala kajakowa.";
	private KeyboardLayout qwerty;

	@Before
	public void setUp() throws IOException {
		Reader corpusReader = new StringReader(CORPUS);
		TextStatistics stats = new TextStatistics(corpusReader);
		qwerty = KeyboardLayout.getQWERTYLayout(stats);
	}

	@Test
	public void testGetPhenotype() {
		final String expected =
			"q w e r t y u i o p\n" +
			"a s d f g h j k l ;\n" +
			"z x c v b n m , . ?";
		assertEquals(expected, qwerty.getPhenotype());
	}

	@Test
	public void testGetPenaltyForFingerUsage() {
		assertEquals(0.021, qwerty.getPenaltyForFingerUsage(), 0.001);
	}

	@Test
	public void testGetPenaltyForRowUsage() {
		assertEquals(0.23, qwerty.getPenaltyForRowUsage(), 0.01);
	}

	@Test
	public void testGetPenaltyForLackOfFingerAlternation() {
		assertEquals(0.27, qwerty.getPenaltyForLackOfFingerAlternation(), 0.01);
	}

	@Test
	public void testGetPenaltyForLackOfHandAlternation() {
		assertEquals(1.24, qwerty.getPenaltyForLackOfHandAlternation(), 0.01);
	}

	@Test
	public void testGetPenaltyForBigSteps() {
		assertEquals(5.21, qwerty.getPenaltyForBigSteps(), 0.01);
	}

	@Test
	public void testGetPenaltyForLackOfInboardStrokeFlow() {
		assertEquals(2.16, qwerty.getPenaltyForLackOfInboardStrokeFlow(), 0.01);
	}

	@Test
	public void testGetPenaltyForHandUsage() {
		assertEquals(0.00043, qwerty.getPenaltyForHandUsage(), 0.00001);
	}

	@Test
	public void testGetDist() {
		assertEquals(0, qwerty.getDist(new Diagraph('a', 'a')));
		assertEquals(1, qwerty.getDist(new Diagraph('q', 'w')));
		assertEquals(1, qwerty.getDist(new Diagraph('q', 'a')));
		assertEquals(1, qwerty.getDist(new Diagraph('i', 'k')));
		assertEquals(1, qwerty.getDist(new Diagraph('v', 'b')));
		assertEquals(2, qwerty.getDist(new Diagraph('q', 'e')));
		assertEquals(2, qwerty.getDist(new Diagraph('u', 'm')));
		assertEquals(3, qwerty.getDist(new Diagraph('w', 'c')));
		assertEquals(4, qwerty.getDist(new Diagraph('y', 'l')));
		assertEquals(8, qwerty.getDist(new Diagraph('x', 'i')));
	}

	@Test
	public void testGetBigFingerStepWeight() {
		assertEquals(5, qwerty.getBigFingerStepWeight(new Diagraph('e', 'f')));
		assertEquals(5, qwerty.getBigFingerStepWeight(new Diagraph('j', 'i')));
		assertEquals(5, qwerty.getBigFingerStepWeight(new Diagraph('j', 'i')));
		assertEquals(9, qwerty.getBigFingerStepWeight(new Diagraph('x', 'e')));
		assertEquals(6, qwerty.getBigFingerStepWeight(new Diagraph('n', 'p')));
		assertEquals(6, qwerty.getBigFingerStepWeight(new Diagraph('a', 'r')));
	}

	@Test
	public void testIsInboardStrokeFlow() {
		assertFalse(qwerty.isInboardStrokeFlow(new Diagraph('a', 'a')));
		assertTrue(qwerty.isInboardStrokeFlow(new Diagraph('a', 's')));
		assertFalse(qwerty.isInboardStrokeFlow(new Diagraph('s', 'a')));
		assertTrue(qwerty.isInboardStrokeFlow(new Diagraph('a', 'r')));
		assertFalse(qwerty.isInboardStrokeFlow(new Diagraph('r', 'a')));
		assertTrue(qwerty.isInboardStrokeFlow(new Diagraph('p', 'o')));
		assertFalse(qwerty.isInboardStrokeFlow(new Diagraph('o', 'p')));
	}

	@Test
	public void testIsLeftRightSide() {
		final String leftSide = "qwertasdfgzxcvb";
		final String rightSide = "yuiophjkl;nm,.?";
		for (int i = 0; i < leftSide.length(); i++) {
			assertTrue(qwerty.isLeftSide(leftSide.charAt(i)));
			assertFalse(qwerty.isLeftSide(rightSide.charAt(i)));
			assertTrue(qwerty.isRightSide(rightSide.charAt(i)));
			assertFalse(qwerty.isRightSide(leftSide.charAt(i)));
		}
	}

	@Test
	public void testIsUpperRow() {
		final String upperRow = "qwertyuiop";
		final String nonUpperRow = "asdfghjkl;zxcvbnm,.?";
		for (int i = 0; i < upperRow.length(); i++) {
			assertTrue(qwerty.isUpperRow(upperRow.charAt(i)));
		}
		for (int i = 0; i < nonUpperRow.length(); i++) {
			assertFalse(qwerty.isUpperRow(nonUpperRow.charAt(i)));
		}
	}

	@Test
	public void testIsMiddleRow() {
		final String middleRow = "asdfghjkl;";
		final String nonMiddleRow = "qwertyuiopzxcvbnm,.?";
		for (int i = 0; i < middleRow.length(); i++) {
			assertTrue(qwerty.isMiddleRow(middleRow.charAt(i)));
		}
		for (int i = 0; i < nonMiddleRow.length(); i++) {
			assertFalse(qwerty.isMiddleRow(nonMiddleRow.charAt(i)));
		}
	}

	@Test
	public void testIsBottomRow() {
		final String bottomRow = "zxcvbnm,.?";
		final String nonBottomRow = "qwertyuioasdfghjkl;";
		for (int i = 0; i < bottomRow.length(); i++) {
			assertTrue(qwerty.isBottomRow(bottomRow.charAt(i)));
		}
		for (int i = 0; i < nonBottomRow.length(); i++) {
			assertFalse(qwerty.isBottomRow(nonBottomRow.charAt(i)));
		}
	}

	@Test
	public void testGetTopRow() {
		assertEquals(stringToList("qwertyuiop"), qwerty.getTopRow());
	}

	@Test
	public void testGetMiddleRow() {
		assertEquals(stringToList("asdfghjkl;"), qwerty.getMiddleRow());
	}

	@Test
	public void testGetBottomRow() {
		assertEquals(stringToList("zxcvbnm,.?"), qwerty.getBottomRow());
	}

	@Test
	public void testRowsPercentage() {
		assertEquals(46.24, qwerty.getUsagePercentage(qwerty.getTopRow()), 0.01);
		assertEquals(27.61, qwerty.getUsagePercentage(qwerty.getMiddleRow()), 0.01);
		assertEquals(26.14, qwerty.getUsagePercentage(qwerty.getBottomRow()), 0.01);
	}

	@Test
	public void testGetLeftHandKeys() {
		assertEquals(stringToList("qwertasdfgzxcvb"), qwerty.getLeftHandKeys());
		assertEquals(stringToList("yuiophjkl;nm,.?"), qwerty.getRightHandKeys());
	}

	@Test
	public void testGetKeysForFinger() {
		assertEquals(stringToList("qaz"), qwerty.getKeysForFinger(0));
		assertEquals(stringToList("wsx"), qwerty.getKeysForFinger(1));
		assertEquals(stringToList("edc"), qwerty.getKeysForFinger(2));
		assertEquals(stringToList("rfvtgb"), qwerty.getKeysForFinger(3));
		assertEquals(stringToList("yhnujm"), qwerty.getKeysForFinger(4));
		assertEquals(stringToList("ik,"), qwerty.getKeysForFinger(5));
		assertEquals(stringToList("ol."), qwerty.getKeysForFinger(6));
		assertEquals(stringToList("p;?"), qwerty.getKeysForFinger(7));
	}

	@Test
	public void testGetFingerForKey() {
		List<Integer> expected = Arrays.asList(
				0, 1, 2, 3, 3, 4, 4, 5, 6, 7,
				0, 1, 2, 3, 3, 4, 4, 5, 6, 7,
				0, 1, 2, 3, 3, 4, 4, 5, 6, 7);
		List<Integer> actual = new ArrayList<Integer>();
		for (int i = 0; i < qwerty.getGenotypeLength(); i++) {
			char c = qwerty.getGeneAt(i);
			int finger = qwerty.getFingerForKey(c);
			actual.add(finger);
		}
		assertEquals(expected, actual);
	}

	@Test
	public void testDescription() {
		final String description = qwerty.getDescription();
		final String expected =
			"rzędy klawiszy: 46,2% 27,6% 26,1%\n" +
			"palce lewej ręki: 16% 9% 15% 11%\n" +
			"palce prawej ręki: 19% 13% 14% 3%\n" +
			"użycie rąk: 51,5% 48,5%\n" +
			"alternacja rąk: 52,7%\n" +
			"zmiana palca: 89,5%";
		assertEquals(expected, description);
	}

	private static List<Character> stringToList(String s) {
		List<Character> chars = new ArrayList<Character>();
		for (int i = 0; i < s.length(); i++) {
			chars.add(s.charAt(i));
		}
		return chars;
	}
}
