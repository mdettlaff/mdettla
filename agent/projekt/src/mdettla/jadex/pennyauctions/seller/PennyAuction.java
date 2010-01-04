package mdettla.jadex.pennyauctions.seller;

import mdettla.jadex.pennyauctions.util.Utils;

public class PennyAuction {

	/**
	 * O ile sekund przedłuża aukcję jedno podbicie.
	 */
	public static final int TIME_UP_PER_BID = 3;
	/**
	 * O ile groszy zwiększa cenę jedno podbicie.
	 */
	public static final int PRICE_UP_PER_BID = 1;

	private static int auctionIdGenerator = 0;

	private Integer auctionId;
	private Product product;
	private Integer currentPrice;
	private Integer timeLeft;
	private User topBidder;
	private boolean isActive;

	public PennyAuction(Product product, Integer currentPrice, Integer timeLeft) {
		this.product = product;
		this.currentPrice = currentPrice;
		this.timeLeft = timeLeft;
		auctionIdGenerator++;
		this.auctionId = auctionIdGenerator;
		this.isActive = true;
	}

	public void setProduct(Product product) {
		this.product = product;
	}

	public Product getProduct() {
		return product;
	}

	public void makeBid(User user) {
		currentPrice += PRICE_UP_PER_BID;
		timeLeft += TIME_UP_PER_BID;
		topBidder = user;
	}

	public Integer getCurrentPrice() {
		return currentPrice;
	}

	public void setTimeLeft(Integer timeLeft) {
		this.timeLeft = timeLeft;
	}

	public Integer getTimeLeft() {
		return timeLeft;
	}

	public void setTopBidder(User topBidder) {
		this.topBidder = topBidder;
	}

	public User getTopBidder() {
		return topBidder;
	}

	public void setId(Integer auctionId) {
		this.auctionId = auctionId;
	}

	public Integer getId() {
		return auctionId;
	}

	public void setActive(boolean isActive) {
		this.isActive = isActive;
	}

	public boolean isActive() {
		return isActive;
	}

	public String toString() {
		return "aukcja nr " + auctionId +
		(topBidder != null ? ", wygrywający: " + topBidder : ", brak wygrywającego") +
		"\n" + "osiągnięta cena " + Utils.formatPrice(currentPrice) + " zł," +
		"towar: " + product;
	}
}
