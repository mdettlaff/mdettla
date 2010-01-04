package pl.edu.agh.jadex;

import jadex.runtime.Plan;
import jadex.util.Tuple;

public class RemoveWordFromDictionary extends Plan {
	private static final long serialVersionUID = 1L;

	public RemoveWordFromDictionary() {}

	public void body() {
		Object[] egwords = getBeliefbase().getBeliefSet("egwords").getFacts();
		getBeliefbase().getBeliefSet("egwords").removeFact(egwords[0]);

		String eword = (String)((Tuple)egwords[0]).get(0);
		String gword = (String)((Tuple)egwords[0]).get(1);
		getLogger().info("usunięto słowo ze słownika: " +
				eword + " " + gword);
	}
}
