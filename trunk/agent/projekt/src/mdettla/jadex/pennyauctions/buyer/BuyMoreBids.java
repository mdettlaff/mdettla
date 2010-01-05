package mdettla.jadex.pennyauctions.buyer;

import mdettla.jadex.pennyauctions.seller.PennyAuction;
import jadex.adapter.fipa.AgentIdentifier;
import jadex.adapter.fipa.SFipa;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;

public class BuyMoreBids extends Plan {
	private static final long serialVersionUID = 1L;

	private static final int BIDS_TO_BUY = 3;

	@Override
	public void body() {
		if (getBeliefbase().getBelief("auction_site").getFact() != null) {
			AgentIdentifier serviceprovider =
				(AgentIdentifier)getBeliefbase().getBelief("auction_site").getFact();
			IMessageEvent me = createMessageEvent("buy_bids");
			StringBuffer content = new StringBuffer("buy_bids");
			content.append(" " + BIDS_TO_BUY);
			content.append(" " + getAgentName());
			me.setContent(content.toString());
			me.getParameterSet(SFipa.RECEIVERS).addValue(serviceprovider);
			sendMessage(me);
			int money = (Integer)getBeliefbase().getBelief("money").getFact();
			getBeliefbase().getBelief("money").setFact(
					money - BIDS_TO_BUY * PennyAuction.BID_PRICE);
			getBeliefbase().getBelief("bids_left").setFact(
					((Integer)getBeliefbase().getBelief("bids_left").getFact()) +
					BIDS_TO_BUY);
		}
	}
}
