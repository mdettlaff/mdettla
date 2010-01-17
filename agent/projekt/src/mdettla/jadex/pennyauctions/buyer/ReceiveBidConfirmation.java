package mdettla.jadex.pennyauctions.buyer;

import jadex.runtime.IExpression;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;
import jadex.util.Tuple;

public class ReceiveBidConfirmation extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		int bidsLeft = (Integer)getBeliefbase().getBelief("bids_left").getFact();
		getBeliefbase().getBelief("bids_left").setFact(bidsLeft - 1);

		Integer auctionId = Integer.valueOf(
				((String)((IMessageEvent)initialevent).getContent()).split(" ")[1]);
		IExpression queryword = getExpression("query_bids_spent_per_auction");
		Integer bidsSpent = (Integer)queryword.execute("$auction_id", auctionId);
		getBeliefbase().getBeliefSet("bids_spent").removeFact(
				new Tuple(auctionId, bidsSpent));
		getBeliefbase().getBeliefSet("bids_spent").addFact(
				new Tuple(auctionId, bidsSpent + 1));
	}
}
