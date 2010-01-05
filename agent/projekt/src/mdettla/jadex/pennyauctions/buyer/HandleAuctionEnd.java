package mdettla.jadex.pennyauctions.buyer;

import jadex.runtime.Plan;

public class HandleAuctionEnd extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		String state = (String)getParameter("state").getValue();
		int currentPrice = Integer.valueOf(state.split(" ")[3]);
		String topBidder = state.split(" ")[4];
		int money = (Integer)getBeliefbase().getBelief("money").getFact();
		if (getAgentName().equals(topBidder)) {
			getBeliefbase().getBelief("money").setFact(money - currentPrice);
		}
		getLogger().info(getAgentName() + ": zostało pieniędzy: " +
				getBeliefbase().getBelief("money").getFact());
		getLogger().info(getAgentName() + ": zostało podbić: " +
				getBeliefbase().getBelief("bids_left").getFact());
	}
}
