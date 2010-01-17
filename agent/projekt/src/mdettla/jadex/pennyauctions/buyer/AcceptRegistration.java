package mdettla.jadex.pennyauctions.buyer;

import jadex.adapter.fipa.AgentIdentifier;
import jadex.adapter.fipa.SFipa;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;

public class AcceptRegistration extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		String content = (String)((IMessageEvent)initialevent).getContent();
		getBeliefbase().getBelief("is_registered").setFact(true);
		IMessageEvent me = (IMessageEvent)initialevent;
		getBeliefbase().getBelief("auction_site").setFact(
				(AgentIdentifier)me.getParameter(SFipa.SENDER).getValue());
		getBeliefbase().getBelief("bid_price").setFact(
				Integer.valueOf(content.split(" ")[1]));
		getBeliefbase().getBelief("bids_in_package").setFact(
				Integer.valueOf(content.split(" ")[2]));
		getLogger().info("potwierdzam zarejestrowanie");
	}
}
