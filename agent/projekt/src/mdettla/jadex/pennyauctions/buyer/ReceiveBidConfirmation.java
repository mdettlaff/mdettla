package mdettla.jadex.pennyauctions.buyer;

import jadex.runtime.Plan;

public class ReceiveBidConfirmation extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		getBeliefbase().getBelief("bids_left").setFact(
				(Integer)getBeliefbase().getBelief("bids_left").getFact() - 1);
	}
}
