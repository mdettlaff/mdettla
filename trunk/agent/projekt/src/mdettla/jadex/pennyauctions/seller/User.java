package mdettla.jadex.pennyauctions.seller;

import jade.core.AID;

public class User {

	public static final int INITIAL_BIDS = 1;

	private String name;
	private AID aid;
	private Integer moneySpent;
	private Integer bidsLeft;

	public User(String name, AID aid) {
		this.name = name;
		this.aid = aid;
		moneySpent = 0;
		bidsLeft = INITIAL_BIDS;
	}

	public String getName() {
		return name;
	}

	public AID getAID() {
		return aid;
	}

	public void buyBids(int bidsCount) {
		bidsLeft += bidsCount;
		moneySpent += PennyAuction.BID_PRICE;
	}

	public Integer getMoneySpent() {
		return moneySpent;
	}

	public void setMoneySpent(Integer moneySpent) {
		this.moneySpent = moneySpent;
	}

	public Integer getBidsLeft() {
		return bidsLeft;
	}

	public void setBidsLeft(Integer bidsLeft) {
		this.bidsLeft = bidsLeft;
	}

	public String toString() {
		return name;
	}
}
