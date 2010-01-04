package mdettla.jadex.pennyauctions.buyer;

import jadex.runtime.Plan;

public class ProcessAuctionState extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		String state = (String)getParameter("state").getValue();
		state = state.split(" ")[1];
		getLogger().info("interpretuję stan aukcji: " + state);
	}
}
