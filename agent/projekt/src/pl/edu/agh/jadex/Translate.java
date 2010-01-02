package pl.edu.agh.jadex;

import jadex.runtime.IExpression;
import jadex.runtime.Plan;
import jadex.util.Tuple;

import java.util.StringTokenizer;

public class Translate extends Plan {
	private static final long serialVersionUID = 1L;

	public Translate() {}

	public void body() {
		String msgContent = (String)getParameter("eword").getValue();
		StringTokenizer st = new StringTokenizer(msgContent);
		st.nextToken();

		while (st.hasMoreTokens()) {
			String eword = st.nextToken();
            IExpression queryword = getExpression("query_egword");
            String gword = (String)queryword.execute("$eword", eword);
			getLogger().info(eword + " -> " + gword);

			getLogger().info("Rozmiar słownika: " +
					getBeliefbase().getBeliefSet("egwords").size());
		}
	}

	public static boolean containsWord(String msgContent, Tuple[] egwords) {
		String eword = msgContent.split(" ")[1];
		for (Tuple egword : egwords) {
			if (egword.get(0).toString().equals(eword)) {
				return true;
			}
		}
		return false;
	}
}
