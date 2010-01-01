package pl.edu.agh.jadex;

import jadex.runtime.Plan;

import java.util.StringTokenizer;

public class AddWordToDictionary extends Plan {
	private static final long serialVersionUID = 1L;

	public AddWordToDictionary() {}

	public void body() {
		String messageContent = (String)getParameter("new_word").getValue();
		StringTokenizer st = new StringTokenizer(messageContent);
		if (st.countTokens() >= 3) {
			st.nextToken();
			String eword = st.nextToken();
			String gword = st.nextToken();
			getBeliefbase().getBeliefSet("egwords").addFact(new jadex.util.Tuple(eword, gword));
			getLogger().info("dodano słowo do słownika: " +
					eword + " " + gword);
		}
	}
}
