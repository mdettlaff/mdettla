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
			dictionary.put("coffee", "Kaffee {m} | Kaffee machen");
			dictionary.put("milk", "Milch {f} | dicke Milch");
			dictionary.put("cow", "Kuh {f} [zool.] | Kuehe {pl}");
			dictionary.put("dog", "Hund {m}");
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
//						gword = gword.replaceAll("</td><td.*", "");
//						gword = gword.trim();
						String gword = getDictionary().get(eword);
						getLogger().info(eword + " -> " + gword);
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
