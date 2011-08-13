package mdettla.keyboard.ga;

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import mdettla.jga.core.Utils;

public class TextStatistics {

	private static final Set<Character> ALL_KEYS = Collections.unmodifiableSet(
			new HashSet<Character>(KeyboardLayout.KEYBOARD_CHARS));

	private final int textLength;
	private Map<Character, Integer> charsFrequencies;
	private Map<Diagraph, Integer> diagraphsFrequencies;

	private final List<Double> optimalRowDistribution;

	public TextStatistics(Reader corpus) throws IOException {
		try {
			charsFrequencies = new HashMap<Character, Integer>();
			diagraphsFrequencies = new HashMap<Diagraph, Integer>();
			int charsReadCount = 0;
			Character prevChar = null;
			int b;
			while ((b = corpus.read()) != -1) {
				char c = (char)b;
				c = normalizeCharacter(c);
				if (!ALL_KEYS.contains(c)) {
					continue;
				}
				if (!charsFrequencies.containsKey(c)) {
					charsFrequencies.put(c, 0);
				}
				charsFrequencies.put(c, charsFrequencies.get(c) + 1);

				if (prevChar != null && !Character.isWhitespace(prevChar)) {
					Diagraph diagraph = new Diagraph(prevChar, c);
					if (!diagraphsFrequencies.containsKey(diagraph)) {
						diagraphsFrequencies.put(diagraph, 0);
					}
					diagraphsFrequencies.put(
							diagraph, diagraphsFrequencies.get(diagraph) + 1);
				}
				prevChar = c;
				charsReadCount++;
			}
			for (Character key : ALL_KEYS) {
				if (!charsFrequencies.containsKey(key)) {
					charsFrequencies.put(key, 0);
				}
			}
			textLength = charsReadCount;
			if (charsFrequencies.size() != ALL_KEYS.size()) {
				throw new IllegalStateException(
						"Wrong size of monographs frequencies map: " + charsFrequencies);
			}
			optimalRowDistribution = computeOptimalRowDistribution();
		} finally {
			corpus.close();
		}
	}

	private char normalizeCharacter(char c) {
		char normalized = Character.toLowerCase(c);
		normalized = Utils.replacePolishChar(normalized);
		normalized = normalized == ':' ? ';' : normalized;
		normalized = normalized == '/' ? '?' : normalized;
		return normalized;
	}

	public int getCharOccurences(char c) {
		return charsFrequencies.get(c);
	}

	public Set<Diagraph> getDiagraphsFound() {
		return diagraphsFrequencies.keySet();
	}

	public int getDiagraphOccurences(Diagraph diagraph) {
		if (diagraphsFrequencies.containsKey(diagraph)) {
			return diagraphsFrequencies.get(diagraph);
		}
		return 0;
	}

	public int getTextLength() {
		return textLength;
	}

	public List<Double> getOptimalRowDistribution() {
		return optimalRowDistribution;
	}

	private List<Double> computeOptimalRowDistribution() {
		List<Character> keysByFreq = getKeysSortedByFrequencyDesc();
		List<Double> distribution = Arrays.asList(0.0, 0.0, 0.0);
		for (int i = 0; i < keysByFreq.size(); i++) {
			char key = keysByFreq.get(i);
			final int row;
			if (i < 10) {
				row = 1;
			} else if (i < 20) {
				row = 0;
			} else {
				row = 2;
			}
			double charFreq = ((double)getCharOccurences(key)) / getTextLength();
			distribution.set(row, distribution.get(row) + charFreq);
		}
		return distribution;
	}

	private List<Character> getKeysSortedByFrequencyDesc() {
		List<Map.Entry<Character, Integer>> entries =
			new ArrayList<Map.Entry<Character, Integer>>(charsFrequencies.entrySet());
		Collections.sort(entries, new Comparator<Map.Entry<Character, Integer>>() {
			@Override
			public int compare(Entry<Character, Integer> e1, Entry<Character, Integer> e2) {
				if (e1.getValue() < e2.getValue()) {
					return 1;
				} else if (e1.getValue() > e2.getValue()) {
					return -1;
				}
				return 0;
			}
		});
		List<Character> keysSortedByFrequencyDesc = new ArrayList<Character>();
		for (Map.Entry<Character, Integer> entry : entries) {
			keysSortedByFrequencyDesc.add(entry.getKey());
		}
		return keysSortedByFrequencyDesc;
	}
}
