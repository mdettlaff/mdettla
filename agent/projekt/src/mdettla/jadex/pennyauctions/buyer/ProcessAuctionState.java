package mdettla.jadex.pennyauctions.buyer;

import mdettla.jadex.pennyauctions.seller.Product;
import mdettla.jadex.pennyauctions.seller.ProductsDatabase;
import jadex.runtime.IMessageEvent;
import jadex.runtime.Plan;

public class ProcessAuctionState extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		String state = (String)getParameter("state").getValue();
		String auctionId = state.split(" ")[1];
		String productId = state.split(" ")[2];
		int currentPrice = Integer.valueOf(state.split(" ")[3]);
		int timeLeft = Integer.valueOf(state.split(" ")[4]);
		String topBidder = state.split(" ")[5];
		if (!topBidder.equals(getAgentName())) { // nie licytyjemy sami ze sobą
			Product product = ProductsDatabase.getProduct(Integer.valueOf(productId));
			if (currentPrice < 0.8 * product.getRetailPrice()
					&& timeLeft < 3) {
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
}
