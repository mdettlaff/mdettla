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
			boolean isLeftRightAlternation =
				isLeftSide(diagraph.getFirstLetter())
				&& isRightSide(diagraph.getSecondLetter());
			boolean isRightLeftAlternation =
				isRightSide(diagraph.getFirstLetter())
				&& isLeftSide(diagraph.getSecondLetter());
			boolean isHandAlternation =
				isLeftRightAlternation || isRightLeftAlternation;
			if (!isHandAlternation) {
				penalty += stats.getDiagraphOccurences(diagraph);
			}
		}
		return penalty;
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
		final int KEYS_PER_ROW = 10;
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
		return getPhenotype();
	}

	public boolean isLeftSide(char c) {
		return keys.indexOf(c) % 10 < 5;
	}

	public boolean isRightSide(char c) {
		return !isLeftSide(c);
	}
}
