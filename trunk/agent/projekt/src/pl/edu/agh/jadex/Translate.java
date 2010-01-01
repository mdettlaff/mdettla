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
			dictionary.put("coffee", "Kaffee");
			dictionary.put("milk", "Milch");
			dictionary.put("cow", "Kuh");
			dictionary.put("dog", "Hund");
//			dictionary.put("cat", "Katze");
		}
		return dictionary;
	}

	public void body() {
		String msgContent = (String)getParameter("eword").getValue();
		StringTokenizer st = new StringTokenizer(msgContent);
		st.nextToken();

		while (st.hasMoreTokens()) {
			String t = st.nextToken();
			String translatedWord = dictionary.get(t);
//            IExpression queryword = getExpression("query_word");
//            String translatedWord = (String)queryword.execute("$word", t);
			getLogger().info(t + " -> " + translatedWord);
		}
	}

	public static boolean containsWord(String name) {
		return dictionary.containsKey(name.split(" ")[1]);
	}
}
