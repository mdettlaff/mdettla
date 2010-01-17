package mdettla.jadex.pennyauctions.buyer;

import java.util.Random;

import mdettla.jadex.pennyauctions.seller.Product;
import mdettla.jadex.pennyauctions.seller.ProductsDatabase;
import jadex.runtime.IExpression;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;
import jadex.util.Tuple;

public class ProcessAuctionState extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		String state = (String)getParameter("state").getValue();
		String auctionId = state.split(" ")[1];
		String productId = state.split(" ")[2];
		int currentPrice = Integer.valueOf(state.split(" ")[3]);
		String topBidder = state.split(" ")[4];
		int timeLeft = Integer.valueOf(state.split(" ")[5]);

		IExpression queryword = getExpression("query_bids_spent_per_auction");
		Integer bidsSpent = (Integer)queryword.execute(
				"$auction_id", Integer.valueOf(auctionId));
		if (bidsSpent == null) {
			getBeliefbase().getBeliefSet("bids_spent").addFact(
					new Tuple(Integer.valueOf(auctionId), 0));
			bidsSpent = 0;
		}

		Product product = ProductsDatabase.getProduct(Integer.valueOf(productId));
		boolean makeBid = true;
		if (topBidder.equals(getAgentName())) { // nie licytyjemy sami ze sobą
			makeBid = false;
		}
		if (((Integer)getBeliefbase().getBelief("bids_left").getFact()) <= 0) {
			makeBid = false;
		}
		int max_price_proc = (Integer)getBeliefbase().getBelief("max_price_proc").getFact();
		if (currentPrice >= (max_price_proc / 100.0) * product.getRetailPrice()) {
			makeBid = false;
		}
		int bidWhenTimeLeft = (Integer)getBeliefbase().getBelief("bid_when_time_left").getFact();
		if (bidWhenTimeLeft < 0) {
			bidWhenTimeLeft = new Random().nextInt(10) + 1;
		}
		if (timeLeft > bidWhenTimeLeft) {
			makeBid = false;
		}
		if (bidsSpent >= (Integer)getBeliefbase().getBelief("max_bids_per_auction").getFact()) {
			makeBid = false;
		}
		// podejmujemy "racjonalną" decyzję
		final int BID_PRICE = (Integer)getBeliefbase().getBelief("bid_price").getFact();
		int profitWhenAuctionLost = (-1) * bidsSpent * BID_PRICE;
		int profitWhenAuctionWon = ((-1) * (bidsSpent + 1) * BID_PRICE)
			- currentPrice + product.getRetailPrice();
		if (profitWhenAuctionLost > profitWhenAuctionWon) {
			makeBid = false;
		}
		if (makeBid) {
			IMessageEvent me = (IMessageEvent)initialevent;
			StringBuffer content = new StringBuffer();
			content.append("bid");
			content.append(" " + auctionId);
			content.append(" " + getAgentName());
			IMessageEvent bid = me.createReply("bid");
			bid.setContent(content.toString());
			sendMessage(bid);
			getLogger().info("podbijam aukcję: " + auctionId);
		}
	}
}
