package mdettla.keyboard.ga;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class KeyboardLayout implements Specimen {

	private static final double WEIGHT_ROW_USAGE = 3.33 * 10;
	private static final double WEIGHT_FINGER_USAGE = 37.17;
	private static final double WEIGHT_HAND_ALTER = 0.0008949 * 0.5;
	private static final double WEIGHT_FINGER_ALTER = 0.00169;
	private static final double WEIGHT_BIG_STEPS = 0.0001919;
	private static final double WEIGHT_INBOARD_STROKE_FLOW = 0.002565 * 0.1;
	private static final double WEIGHT_HAND_USAGE = 71.42;

	static final List<Character> KEYBOARD_CHARS = Arrays.asList(
			'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
			'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
			'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '?'
	);
	final int KEYS_PER_ROW = 10;
	public final Objective[] OBJECTIVES = {
			new Objective() {
				@Override
				public String getName() {
					return "row usage";
				}
				@Override
				public double computeValue() {
					return getPenaltyForRowUsage();
				}
				@Override
				public double getWeight() {
					return WEIGHT_ROW_USAGE;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "finger usage";
				}
				@Override
				public double computeValue() {
					return getPenaltyForFingerUsage();
				}
				@Override
				public double getWeight() {
					return WEIGHT_FINGER_USAGE;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "hand alternation";
				}
				@Override
				public double computeValue() {
					return getPenaltyForLackOfHandAlternation();
				}
				@Override
				public double getWeight() {
					return WEIGHT_HAND_ALTER;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "finger alternation";
				}
				@Override
				public double computeValue() {
					return getPenaltyForLackOfFingerAlternation();
				}
				@Override
				public double getWeight() {
					return WEIGHT_FINGER_ALTER;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "big steps";
				}
				@Override
				public double computeValue() {
					return getPenaltyForBigSteps();
				}
				@Override
				public double getWeight() {
					return WEIGHT_BIG_STEPS;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "inboard stroke flow";
				}
				@Override
				public double computeValue() {
					return getPenaltyForLackOfInboardStrokeFlow();
				}
				@Override
				public double getWeight() {
					return WEIGHT_INBOARD_STROKE_FLOW;
				}
			},
			new Objective() {
				@Override
				public String getName() {
					return "hand usage";
				}
				@Override
				public double computeValue() {
					return getPenaltyForHandUsage();
				}
				@Override
				public double getWeight() {
					return WEIGHT_HAND_USAGE;
				}
			}
	};

	private List<Character> keys;
	private final TextStatistics stats;
	private Double fitness;

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
	public void computeFitness() {
		if (fitness != null) {
			throw new IllegalStateException("Fitness was already computed.");
		}
		checkIntegrity();
		double penalty = 0;
		for (Objective objective : OBJECTIVES) {
			penalty += objective.getValue() * objective.getWeight();
		}
		fitness = penalty;
	}

	@Override
	public Double getFitness() {
		if (fitness == null) {
			throw new IllegalStateException("Must compute fitness first.");
		}
		return fitness;
	}

	public static KeyboardLayout getQWERTYLayout(TextStatistics stats) {
		final List<Character> QWERTY_CHARS = Arrays.asList(
				'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
				'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
				'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '?'
		);
		KeyboardLayout qwerty = new KeyboardLayout(stats);
		qwerty.keys = QWERTY_CHARS;
		qwerty.computeFitness();
		return qwerty;
	}

	public static KeyboardLayout getDvorakLayout(TextStatistics stats) {
		final List<Character> DVORAK_CHARS = Arrays.asList(
				'?', ',', '.', 'p', 'y', 'f', 'g', 'c', 'r', 'l',
				'a', 'o', 'e', 'u', 'i', 'd', 'h', 't', 'n', 's',
				';', 'q', 'j', 'k', 'x', 'b', 'm', 'w', 'v', 'z'
		);
		KeyboardLayout dvorak = new KeyboardLayout(stats);
		dvorak.keys = DVORAK_CHARS;
		dvorak.computeFitness();
		return dvorak;
	}

//	private int getPenaltyForLocation() {
//		int[] costs = {
//				5, 3, 3, 3, 4, 4, 3, 3, 3, 5,
//				1, 0, 0, 0, 2, 2, 0, 0, 0, 1,
//				6, 5, 5, 5, 7, 7, 5, 5, 5, 6
//		};
//		int penalty = 0;
//		for (int i = 0; i < KEYBOARD_CHARS.size(); i++) {
//			char c = keys.get(i);
//			penalty += costs[i] * stats.getCharOccurences(c);
//		}
//		return penalty;
//	}

	double getPenaltyForRowUsage() {
		List<Double> optimalDistribution = stats.getOptimalRowDistribution();
		List<Double> actualDistribution = Arrays.asList(0.0, 0.0, 0.0);
		for (char key : keys) {
			int row = getRowForKey(key);
			double charFreq = ((double)stats.getCharOccurences(key)) / stats.getTextLength();
			actualDistribution.set(row, actualDistribution.get(row) + charFreq);
		}
		return Utils.vectorDiff(optimalDistribution, actualDistribution);
	}

	double getPenaltyForFingerUsage() {
		List<Double> optimalDistribution =
			Arrays.asList(0.044, 0.123, 0.158, 0.175, 0.175, 0.158, 0.123, 0.044);
		List<Double> actualDistribution =
			Arrays.asList(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);
		for (char key : keys) {
			int finger = getFingerForKey(key);
			double charFreq = ((double)stats.getCharOccurences(key)) / stats.getTextLength();
			actualDistribution.set(finger, actualDistribution.get(finger) + charFreq);
		}
		return Utils.vectorDiff(optimalDistribution, actualDistribution);
	}

	double getPenaltyForLackOfHandAlternation() {
		double penalty = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			if (!isHandAlternation(diagraph)) {
				penalty += getDiagraphFreq(diagraph);
			}
		}
		return penalty;
	}

	double getPenaltyForLackOfFingerAlternation() {
		double penalty = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			if (!isFingerAlternation(diagraph)) {
				penalty += getDiagraphFreq(diagraph);
			}
		}
		return penalty;
	}

	double getPenaltyForBigSteps() {
		double penalty = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			boolean sameHand = !isHandAlternation(diagraph);
			boolean differentFinger = getFingerForKey(diagraph.getFirstLetter())
					!= getFingerForKey(diagraph.getSecondLetter());
			int verticalDist = Math.abs(getRowForKey(diagraph.getFirstLetter())
					- getRowForKey(diagraph.getSecondLetter()));
			if (sameHand && differentFinger && verticalDist > 0) {
				penalty += getDiagraphFreq(diagraph) * getBigFingerStepWeight(diagraph);
			}
		}
		return penalty;
	}

	double getPenaltyForLackOfInboardStrokeFlow() {
		double penalty = 0;
		for (Diagraph diagraph : stats.getDiagraphsFound()) {
			if (!isInboardStrokeFlow(diagraph)) {
				penalty += getDiagraphFreq(diagraph);
			}
		}
		return penalty;
	}

	double getPenaltyForHandUsage() {
		List<Double> optimalDistribution = Arrays.asList(0.475, 0.525);
		List<Double> actualDistribution = Arrays.asList(0.0, 0.0);
		for (char key : keys) {
			double charFreq = ((double)stats.getCharOccurences(key)) / stats.getTextLength();
			int hand = isLeftSide(key) ? 0 : 1;
			actualDistribution.set(hand, actualDistribution.get(hand) + charFreq);
		}
		return Utils.vectorDiff(optimalDistribution, actualDistribution);
	}

	private double getDiagraphFreq(Diagraph diagraph) {
		double diagraphOccurences = stats.getDiagraphOccurences(diagraph);
		return diagraphOccurences / stats.getDiagraphsFound().size();
	}

	int getDist(Diagraph d) {
		int c1 = keys.indexOf(d.getFirstLetter()) % KEYS_PER_ROW;
		int c2 = keys.indexOf(d.getSecondLetter()) % KEYS_PER_ROW;
		int r1 = keys.indexOf(d.getFirstLetter()) / KEYS_PER_ROW;
		int r2 = keys.indexOf(d.getSecondLetter()) / KEYS_PER_ROW;
		return Math.abs(c2 - c1) + Math.abs(r2 - r1);
	}

	int getBigFingerStepWeight(Diagraph diagraph) {
		int[][] weights = {
				{0, 10, 7, 6, 6, 7, 10, 0},
				{10, 0, 9, 8, 8, 9, 0, 10},
				{7, 9, 0, 5, 5, 0, 9, 7},
				{6, 8, 5, 0, 0, 5, 8, 6},
				{6, 8, 5, 0, 0, 5, 8, 6},
				{7, 9, 0, 5, 5, 0, 9, 7},
				{10, 0, 9, 8, 8, 9, 0, 10},
				{0, 10, 7, 6, 6, 7, 10, 0}
		};
		int finger1 = getFingerForKey(diagraph.getFirstLetter());
		int finger2 = getFingerForKey(diagraph.getSecondLetter());
		return weights[finger1][finger2];
	}

	boolean isInboardStrokeFlow(Diagraph diagraph) {
		int column1 = keys.indexOf(diagraph.getFirstLetter()) % KEYS_PER_ROW;
		int column2 = keys.indexOf(diagraph.getSecondLetter()) % KEYS_PER_ROW;
		if (isLeftSide(diagraph.getFirstLetter())
				&& isLeftSide(diagraph.getSecondLetter())) {
			return column1 < column2;
		} else if (isRightSide(diagraph.getFirstLetter())
				&& isRightSide(diagraph.getSecondLetter())) {
			return column2 < column1;
		}
		return false;
	}

	private int getRowForKey(char key) {
		if (isUpperRow(key)) {
			return 0;
		} else if (isMiddleRow(key)) {
			return 1;
		} else {
			return 2;
		}
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
		if (getFitness().doubleValue() < other.getFitness().doubleValue()) {
			return 1;
		} else if (getFitness().doubleValue() > other.getFitness().doubleValue()) {
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
		getDescription() + "\n"	+ getEvaluationFunctionComponents();
	}

	private String getEvaluationFunctionComponents() {
		StringBuilder evaluationFunction = new StringBuilder();
		for (Objective objective : OBJECTIVES) {
			evaluationFunction.append(objective.getName() + " = " +
					(objective.getValue() * objective.getWeight()) + "\n");
		}
		return evaluationFunction.toString().trim();
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
