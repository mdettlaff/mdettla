package pl.edu.agh.jadex;

import jadex.runtime.Plan;
import java.net.MalformedURLException;
import java.net.URL;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

@SuppressWarnings("serial")
public class SearchTranslationOnline extends Plan {

	public SearchTranslationOnline() {
	}

	public void body() {
		String eword = (String) getParameter("eword").getValue();
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
				if (inline.indexOf("<td>") != -1 && inline.indexOf(eword) != -1) {
					try
					{
						int start = inline.indexOf("<td>") + 4;
						int end = inline.indexOf("</td", start);
						String worda = inline.substring(start, end);
						start = inline.indexOf("<td", start);
						start = inline.indexOf(">", start);
						end = inline.indexOf("</td", start);
						String wordb = inline.substring(start,
								end == -1 ? inline.length() - 1 : end);
						wordb = wordb.replaceAll("<b>", "");
						wordb = wordb.replaceAll("</b>", "");
						System.out.println(worda + " - " + wordb);
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
