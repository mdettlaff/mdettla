package mdettla.keyboard.ga;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.Specimen;

public class KeyboardLayout implements Specimen {

	private static final List<Character> KEYBOARD_CHARS = Arrays.asList(
			'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
			'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
			'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '?'
	);
	final int KEYS_PER_ROW = 10;

	private List<Character> keys;
	private final TextStatistics stats;

	KeyboardLayout(TextStatistics stats) {
		keys = new ArrayList<Character>(KEYBOARD_CHARS);
		this.stats = stats;
	}

	public static Specimen createRandomInstance(TextStatistics stats) {
		Specimen randomSpecimen = new KeyboardLayout(stats);
		List<Character> randomKeys = new ArrayList<Character>(KEYBOARD_CHARS);
		Collections.shuffle(randomKeys);
		for (int i = 0; i < randomSpecimen.getGenotypeLength(); i++) {
			randomSpecimen.setGeneAt(i, randomKeys.get(i));
		}
		return randomSpecimen;
	}

	@Override
	public KeyboardLayout createCopy() {
		KeyboardLayout layout = new KeyboardLayout(stats);
		for (int i = 0; i < getGenotypeLength(); i++) {
			layout.setGeneAt(i, this.getGeneAt(i));
		}
		return layout;
	}

	@Override
	public int getGenotypeLength() {
		return KEYBOARD_CHARS.size();
	}

	@Override
	public void setGeneAt(int position, Object gene) {
		keys.set(position, (Character)gene);
	}

	@Override
	public Character getGeneAt(int position) {
		return keys.get(position);
	}

	@Override
	public void setRandomGeneValueAt(int position) {
		Random random = new Random();
		keys.set(position,
				KEYBOARD_CHARS.get(random.nextInt(KEYBOARD_CHARS.size())));
	}

	@Override
	public void setOppositeGeneValueAt(int position) {
		Random random = new Random();
		int shift = random.nextInt(KEYBOARD_CHARS.size() - 1) + 1;
		keys.set(position, KEYBOARD_CHARS.get(
				(KEYBOARD_CHARS.indexOf(keys.get(position)) + shift)
				% KEYBOARD_CHARS.size()));
	}

	@Override
	public Integer getFitness() {
		checkIntegrity();
		int penalty = 0;
		penalty += getPenaltyForLocation();
		penalty += getPenaltyForLackOfHandAlternation();
		return penalty;
	}

	private int getPenaltyForLocation() {
		int[] costs = {
				5, 3, 3, 3, 4, 4, 3, 3, 3, 5,
				1, 0, 0, 0, 2, 2, 0, 0, 0, 1,
				6, 5, 5, 5, 7, 7, 5, 5, 5, 6
		};
		int penalty = 0;
		for (int i = 0; i < KEYBOARD_CHARS.size(); i++) {
			char c = keys.get(i);
			penalty += costs[i] * stats.getCharOccurences(c);
		}
		return penalty;
	}

	private int getPenaltyForLackOfHandAlternation() {
		int penalty = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			if (!isHandAlternation(diagraph)) {
				penalty += stats.getDiagraphOccurences(diagraph);
			}
		}
		return penalty;
	}

	private boolean isHandAlternation(Diagraph diagraph) {
		boolean isLeftRightAlternation =
			isLeftSide(diagraph.getFirstLetter())
			&& isRightSide(diagraph.getSecondLetter());
		boolean isRightLeftAlternation =
			isRightSide(diagraph.getFirstLetter())
			&& isLeftSide(diagraph.getSecondLetter());
		boolean isHandAlternation =
			isLeftRightAlternation || isRightLeftAlternation;
		return isHandAlternation;
	}

	private void checkIntegrity() {
		boolean isPermutation = keys.size() == new HashSet<Character>(keys).size();
		if (!isPermutation) {
			throw new IllegalStateException("The layout:\n" + this + " is not valid.");
		}
	}

	/**
	 * Mniejsza wartość jest lepsza.
	 */
	@Override
	public int compareTo(Specimen other) {
		if (getFitness().intValue() < other.getFitness().intValue()) {
			return 1;
		} else if (getFitness().intValue() > other.getFitness().intValue()) {
			return -1;
		} else {
			return 0;
		}
	}

	@Override
	public String getPhenotype() {
		StringBuilder phenotype = new StringBuilder();
		final int KEY_ROWS_COUNT = 3;
		for (int i = 0; i < KEY_ROWS_COUNT; i++) {
			for (int j = 0; j < KEYS_PER_ROW; j++) {
				phenotype.append(keys.get(i * KEYS_PER_ROW + j) + " ");
			}
			phenotype.setCharAt(phenotype.length() - 1, '\n');
		}
		return phenotype.toString().trim();
	}

	@Override
	public String toString() {
		return getPhenotype() + "\n" +
		"przystosowanie: " + getFitness() + "\n" +
		getDescription();
	}

	String getDescription() {
		final String rowsUsage =
			String.format("rzędy klawiszy: %.1f%% %.1f%% %.1f%%",
					getUsagePercentage(getTopRow()),
					getUsagePercentage(getMiddleRow()),
					getUsagePercentage(getBottomRow()));
		final String leftHandFingersUsage =
			String.format("palce lewej ręki: %.0f%% %.0f%% %.0f%% %.0f%%",
					getUsagePercentage(getKeysForFinger(0)),
					getUsagePercentage(getKeysForFinger(1)),
					getUsagePercentage(getKeysForFinger(2)),
					getUsagePercentage(getKeysForFinger(3)));
		final String rightHandFingersUsage =
			String.format("palce prawej ręki: %.0f%% %.0f%% %.0f%% %.0f%%",
					getUsagePercentage(getKeysForFinger(4)),
					getUsagePercentage(getKeysForFinger(5)),
					getUsagePercentage(getKeysForFinger(6)),
					getUsagePercentage(getKeysForFinger(7)));
		final String handsUsage =
			String.format("użycie rąk: %.1f%% %.1f%%",
					getUsagePercentage(getLeftHandKeys()),
					getUsagePercentage(getRightHandKeys()));
		final String handAlternation =
			String.format("alternacja rąk: %.1f%%", getHandAlternation());
		final String fingerAlternation =
			String.format("zmiana palca: %.1f%%", getFingerAlternation());
		return rowsUsage + "\n" +
		leftHandFingersUsage + "\n" + rightHandFingersUsage + "\n" +
		handsUsage + "\n" + handAlternation + "\n" + fingerAlternation;
	}

	List<Character> getKeysForFinger(int finger) {
		switch(finger) {
		case 0:
			return Arrays.asList(keys.get(0), keys.get(10), keys.get(20));
		case 1:
			return Arrays.asList(keys.get(1), keys.get(11), keys.get(21));
		case 2:
			return Arrays.asList(keys.get(2), keys.get(12), keys.get(22));
		case 3:
			return Arrays.asList(keys.get(3), keys.get(13), keys.get(23),
					keys.get(4), keys.get(14), keys.get(24));
		case 4:
			return Arrays.asList(keys.get(5), keys.get(15), keys.get(25),
					keys.get(6), keys.get(16), keys.get(26));
		case 5:
			return Arrays.asList(keys.get(7), keys.get(17), keys.get(27));
		case 6:
			return Arrays.asList(keys.get(8), keys.get(18), keys.get(28));
		case 7:
			return Arrays.asList(keys.get(9), keys.get(19), keys.get(29));
		default:
			throw new IllegalArgumentException("Unknown finger: " + finger);
		}
	}

	private double getHandAlternation() {
		int allOccurencesCount = 0;
		int alternationOccurencesCount = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			int diagraphOccurences = stats.getDiagraphOccurences(diagraph);
			allOccurencesCount += diagraphOccurences;
			if (isHandAlternation(diagraph)) {
				alternationOccurencesCount += diagraphOccurences;
			}
		}
		return getPercentage(alternationOccurencesCount, allOccurencesCount);
	}

	private double getFingerAlternation() {
		int allOccurencesCount = 0;
		int alternationOccurencesCount = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			int diagraphOccurences = stats.getDiagraphOccurences(diagraph);
			allOccurencesCount += diagraphOccurences;
			if (isFingerAlternation(diagraph)) {
				alternationOccurencesCount += diagraphOccurences;
			}
		}
		return getPercentage(alternationOccurencesCount, allOccurencesCount);
	}

	private boolean isFingerAlternation(Diagraph diagraph) {
		return getFingerForKey(diagraph.getFirstLetter())
				!= getFingerForKey(diagraph.getSecondLetter());
	}

	boolean isLeftSide(char c) {
		return keys.indexOf(c) % KEYS_PER_ROW < 5;
	}

	boolean isRightSide(char c) {
		return !isLeftSide(c);
	}

	boolean isUpperRow(char c) {
		return keys.indexOf(c) < KEYS_PER_ROW;
	}

	boolean isMiddleRow(char c) {
		int charIndex = keys.indexOf(c);
		return KEYS_PER_ROW <= charIndex && charIndex < KEYS_PER_ROW * 2;
	}

	boolean isBottomRow(char c) {
		return KEYS_PER_ROW * 2 <= keys.indexOf(c);
	}

	double getUsagePercentage(List<Character> row) {
		int allOccurencesCount = getOccurencesCount(keys);
		int occurencesForRowCount = getOccurencesCount(row);
		return getPercentage(occurencesForRowCount, allOccurencesCount);
	}

	private double getPercentage(int occurencesCount, int allOccurencesCount) {
		if (allOccurencesCount == 0) {
			return 0;
		}
		return ((double)occurencesCount) / allOccurencesCount * 100;
	}

	private int getOccurencesCount(List<Character> chars) {
		int allOccurencesCount = 0;
		for (char c : chars) {
			allOccurencesCount += stats.getCharOccurences(c);
		}
		return allOccurencesCount;
	}

	List<Character> getTopRow() {
		return keys.subList(0, KEYS_PER_ROW);
	}

	List<Character> getMiddleRow() {
		return keys.subList(KEYS_PER_ROW, 2 * KEYS_PER_ROW);
	}

	List<Character> getBottomRow() {
		return keys.subList(2 * KEYS_PER_ROW, 3 * KEYS_PER_ROW);
	}

	List<Character> getLeftHandKeys() {
		List<Character> leftHandKeys = new ArrayList<Character>();
		for (int i = 0; i < keys.size(); i++) {
			char c = keys.get(i);
			if (isLeftSide(c)) {
				leftHandKeys.add(c);
			}
		}
		return leftHandKeys;
	}

	List<Character> getRightHandKeys() {
		List<Character> rightHandKeys = new ArrayList<Character>();
		for (int i = 0; i < keys.size(); i++) {
			char c = keys.get(i);
			if (isRightSide(c)) {
				rightHandKeys.add(c);
			}
		}
		return rightHandKeys;
	}

	int getFingerForKey(char c) {
		int index = keys.indexOf(c) % KEYS_PER_ROW;
		if (index < 3) {
			return index;
		} else if (index < 5) {
			return 3;
		} else if (index < 7) {
			return 4;
		} else {
			return index - 2;
		}
	}
}
