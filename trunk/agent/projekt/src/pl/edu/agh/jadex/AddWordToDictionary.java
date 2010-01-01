package pl.edu.agh.jadex;

import jadex.runtime.Plan;

import java.util.Map;
import java.util.StringTokenizer;

public class AddWordToDictionary extends Plan {
	private static final long serialVersionUID = 1L;

	public AddWordToDictionary() {}

	@SuppressWarnings("unchecked")
	public void body() {
		Map<String, String> words = (Map<String, String>)getBeliefbase().
		getBelief("dictionary").getFact();
		String messageContent = (String)getParameter("new_word").getValue();
		StringTokenizer st = new StringTokenizer(messageContent);
		if (st.countTokens() >= 3) {
			st.nextToken();
			String key = st.nextToken();
			if (!words.containsKey(key)) {
				words.put(key, st.nextToken());
				getLogger().info("dodano słowo do słownika");
			}
		}
	}
}
