package mdettla.jadex.pennyauctions.buyer;

import jadex.runtime.Plan;

public class ReceiveBidConfirmation extends Plan {
	private static final long serialVersionUID = 1L;

	@Override
	public void body() {
		int bidsLeft = (Integer)getBeliefbase().getBelief("bids_left").getFact();
		getBeliefbase().getBelief("bids_left").setFact(bidsLeft - 1);
		int bidsSpent = ((Integer)getBeliefbase().getBelief("bids_spent").getFact());
		getBeliefbase().getBelief("bids_spent").setFact(
				bidsSpent + 1);
	}
}
