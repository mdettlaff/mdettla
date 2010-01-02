package pl.edu.agh.jadex;

import jadex.runtime.Plan;
import jadex.util.Tuple;

public class RemoveWordFromDictionary extends Plan {
	private static final long serialVersionUID = 1L;

	public RemoveWordFromDictionary() {}

	public void body() {
		Tuple[] egwords = (Tuple[])getBeliefbase().getBeliefSet("egwords").getFacts();
		String eword = (String)egwords[0].getEntity(0);
		String gword = (String)egwords[0].getEntity(1);
		getBeliefbase().getBeliefSet("egwords").removeFact(new Tuple(eword, gword));
		getLogger().info("usunięto słowo ze słownika: " +
				eword + " " + gword);
	}
}
