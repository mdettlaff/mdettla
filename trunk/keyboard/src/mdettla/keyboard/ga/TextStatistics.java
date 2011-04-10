package mdettla.keyboard.ga;

import java.io.IOException;
import java.io.Reader;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class TextStatistics {

	private Map<Character, Integer> charsFrequencies;
	private Map<Diagraph, Integer> diagraphsFrequencies;

	public TextStatistics(Reader corpus) throws IOException {
		try {
			charsFrequencies = new HashMap<Character, Integer>();
			diagraphsFrequencies = new HashMap<Diagraph, Integer>();
			Character prevChar = null;
			int b;
			while ((b = corpus.read()) != -1) {
				char c = (char)b;
				c = Character.toLowerCase(c);
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
			}
		} finally {
			corpus.close();
		}
	}

	public int getCharOccurences(char c) {
		if (charsFrequencies.containsKey(c)) {
			return charsFrequencies.get(c);
		}
		return 0;
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
}
