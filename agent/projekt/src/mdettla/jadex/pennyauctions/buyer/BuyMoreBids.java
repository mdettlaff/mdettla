package mdettla.jadex.pennyauctions.buyer;

import jadex.adapter.fipa.AgentIdentifier;
import jadex.adapter.fipa.SFipa;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;
import jadex.util.Tuple;

public class BuyMoreBids extends Plan {
	private static final long serialVersionUID = 1L;

	private static final int BID_PACKAGES_TO_BUY = 1;

	@Override
	public void body() {
		if (getBeliefbase().getBelief("auction_site").getFact() != null) {
			AgentIdentifier auctionSite =
				(AgentIdentifier)getBeliefbase().getBelief("auction_site").getFact();
			IMessageEvent me = createMessageEvent("buy_bids");
			StringBuffer content = new StringBuffer("buy_bids");
			content.append(" " + BID_PACKAGES_TO_BUY);
			content.append(" " + getAgentName());
			me.setContent(content.toString());
			me.getParameterSet(SFipa.RECEIVERS).addValue(auctionSite);
			sendMessage(me);
			int money = (Integer)getBeliefbase().getBelief("money").getFact();
			final int BID_PRICE = (Integer)getBeliefbase().getBelief("bid_price").getFact();
			final int BIDS_IN_PACKAGE =
				(Integer)getBeliefbase().getBelief("bids_in_package").getFact();
			getBeliefbase().getBelief("money").setFact(
					money - BID_PRICE * BID_PACKAGES_TO_BUY * BIDS_IN_PACKAGE);
			int bidsLeft = ((Integer)getBeliefbase().getBelief("bids_left").getFact());
			getBeliefbase().getBelief("bids_left").setFact(
					bidsLeft + BID_PACKAGES_TO_BUY * BIDS_IN_PACKAGE);
		}
	}

	public static int getMaxBidsSpent(Tuple[] bidsSpent) {
		int maxBidsSpent = 0 ;
		for (Tuple bidsSpentPerAuction : bidsSpent) {
			maxBidsSpent = Math.max((Integer)bidsSpentPerAuction.get(1), maxBidsSpent);
		}
		return maxBidsSpent;
	}
}
