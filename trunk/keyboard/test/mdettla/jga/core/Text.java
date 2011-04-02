package mdettla.jga.core;

import java.util.Random;

class Text implements Specimen {

	public static final String TARGET = "methinks it is like a weasel";

	public static final String AVAILABLE_CHARS = "abcdefghijklmnopqrstuvwxyz ";

	private char[] text;

	public Text() {
		text = new char[getGenotypeLength()];
	}

	public static Specimen createRandomInstance() {
		Specimen randomSpecimen = new Text();
		for (int i = 0; i < randomSpecimen.getGenotypeLength(); i++) {
			randomSpecimen.setRandomGeneValueAt(i);
		}
		return randomSpecimen;
	}

	@Override
	public Specimen createCopy() {
		Specimen specimen = new Text();
		for (int i = 0; i < getGenotypeLength(); i++) {
			specimen.setGeneAt(i, this.getGeneAt(i));
		}
		return specimen;
	}

	@Override
	public int getGenotypeLength() {
		return TARGET.length();
	}

	@Override
	public void setGeneAt(int position, Object gene) {
		text[position] = (Character)gene;
	}

	@Override
	public Character getGeneAt(int position) {
		return text[position];
	}

	@Override
	public void setRandomGeneValueAt(int position) {
		Random random = new Random();
		text[position] = AVAILABLE_CHARS.charAt(
				random.nextInt(AVAILABLE_CHARS.length()));
	}

	@Override
	public void setOppositeGeneValueAt(int position) {
		Random random = new Random();
		int shift = random.nextInt(AVAILABLE_CHARS.length() - 1) + 1;
		text[position] = AVAILABLE_CHARS.charAt(
				(AVAILABLE_CHARS.indexOf(text[position]) + shift)
				% AVAILABLE_CHARS.length());
	}

	@Override
	public Integer getFitness() {
		Integer matchingChars = 0;
		for (int i = 0; i < TARGET.length(); i++) {
			if (text[i] == TARGET.charAt(i)) {
				matchingChars++;
			}
		}
		return matchingChars;
	}

	@Override
	public int compareTo(Specimen other) {
		if (getFitness().intValue() > other.getFitness().intValue()) {
			return 1;
		} else if (getFitness().intValue() < other.getFitness().intValue()) {
			return -1;
		} else {
			return 0;
		}
	}

	public String getPhenotype() {
		StringBuffer phenotype = new StringBuffer();
		phenotype.append("\"");
		for (int i = 0; i < text.length; i++) {
			phenotype.append(text[i]);
		}
		phenotype.append("\"");
		return phenotype.toString();
	}

	@Override
	public String toString() {
		return getPhenotype();
	}
}
