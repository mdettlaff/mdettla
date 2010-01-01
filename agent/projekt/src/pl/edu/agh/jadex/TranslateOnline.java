package pl.edu.agh.jadex;

import jadex.runtime.Plan;
import java.net.MalformedURLException;
import java.net.URL;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

@SuppressWarnings("serial")
public class TranslateOnline extends Plan {

	public TranslateOnline() {
	}

	public void body() {
		String eword = (String) getParameter("word").getValue();
		eword = eword.split(" ")[1];
		URL dict = null;
		try {
			dict = new URL("http://wolfram.schneider.org/dict/dict.cgi?query="
					+ eword);
		} catch (MalformedURLException e) {
			e.printStackTrace();
		}
		BufferedReader in = null;
		try {
			in = new BufferedReader(new InputStreamReader(dict.openStream()));
		} catch (IOException e) {
			e.printStackTrace();
		}
		String inline = null;
		try {
			while ((inline = in.readLine()) != null) {
				if (inline.indexOf("<td><b>" + eword + "</b> ") != -1) {
					try {
						String englishWord = inline.replaceAll(".*right\">", "");
						englishWord = englishWord.trim();
						getLogger().info(eword + " -> " + englishWord);
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
			}
			in.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
