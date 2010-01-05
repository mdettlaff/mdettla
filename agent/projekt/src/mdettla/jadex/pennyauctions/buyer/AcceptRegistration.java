package mdettla.jadex.pennyauctions.buyer;

import jadex.adapter.fipa.AgentIdentifier;
import jadex.adapter.fipa.SFipa;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;

public class AcceptRegistration extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		getBeliefbase().getBelief("is_registered").setFact(true);
		IMessageEvent me = (IMessageEvent)initialevent;
		getBeliefbase().getBelief("auction_site").setFact(
				(AgentIdentifier)me.getParameter(SFipa.SENDER).getValue());
		getLogger().info("potwierdzam zarejestrowanie");
	}
}
