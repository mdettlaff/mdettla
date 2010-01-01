package pl.edu.agh.jadex;

import jadex.runtime.Plan;

import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("serial")
public class TranslateOnline extends Plan {

	protected static Map<String, String> dictionary;

	public TranslateOnline() {}

	public static Map<String, String> getDictionary() {
		if (dictionary == null) {
			dictionary = new HashMap<String, String>();
			dictionary.put("coffee", "Kaffee");
			dictionary.put("milk", "Milch");
			dictionary.put("cow", "Kuh");
			dictionary.put("dog", "Hund");
			dictionary.put("cat", "Katze {f} [zool.] | Katzen {pl}");
		}
		return dictionary;
	}

	public void body() {
		String eword = (String) getParameter("eword").getValue();
		eword = eword.split(" ")[1];
//		URL dict = null;
//		try {
//			dict = new URL("http://wolfram.schneider.org/dict/dict.cgi?wholewords=1&query="
//					+ eword);
//		} catch (MalformedURLException e) {
//			e.printStackTrace();
//		}
//		BufferedReader in = null;
//		try {
//			in = new BufferedReader(new InputStreamReader(dict.openStream()));
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		String inline = null;
//		try {
//			while ((inline = in.readLine()) != null) {
//				if (inline.indexOf("><b>" + eword + "</b> |") != -1) {
//					try {
//						String gword = inline.replaceAll(".*left\"><td>", "");
//						gword = gword.replaceAll("<td>.*", "");
//						gword = gword.trim();
						getLogger().info(eword + " -> " + getDictionary().get(eword));
//					} catch (Exception e) {
//						e.printStackTrace();
//					}
//				}
//			}
//			in.close();
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
	}
}
