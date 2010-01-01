package pl.edu.agh.jadex;

import jadex.runtime.Plan;

import java.util.HashMap;
import java.util.Map;
import java.util.StringTokenizer;

public class Translate extends Plan {
	private static final long serialVersionUID = 1L;

	protected static Map<String, String> dictionary;

	public Translate() {}

	public static Map<String, String> getDictionary() {
		if (dictionary == null) {
			dictionary = new HashMap<String, String>();
			dictionary.put("Kaffee", "coffee");
			dictionary.put("Milch", "milk");
			dictionary.put("Kuh", "cow");
			dictionary.put("Katze", "cat");
			//dictionary.put("Hund", "dog");
		}
		return dictionary;
	}

	public void body() {
		String msgContent = (String)getParameter("word").getValue();
		StringTokenizer st = new StringTokenizer(msgContent);
		st.nextToken();

		while (st.hasMoreTokens()) {
			String t = st.nextToken();
			getLogger().info(t + " -> " + dictionary.get(t));
		}
	}

	public static boolean containsWord(String name) {
		return dictionary.containsKey(name);
	}
}
