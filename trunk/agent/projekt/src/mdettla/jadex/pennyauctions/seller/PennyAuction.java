package mdettla.jadex.pennyauctions.seller;

public class PennyAuction {

	private Product product;
	private Integer currentPrice;
	private Integer timeLeft;
	private String topBidderName;

	public PennyAuction(Product product, Integer currentPrice, Integer timeLeft) {
		this.product = product;
		this.currentPrice = currentPrice;
		this.timeLeft = timeLeft;
	}

	public void setProduct(Product product) {
		this.product = product;
	}

	public Product getProduct() {
		return product;
	}

	public void setCurrentPrice(Integer currentPrice) {
		this.currentPrice = currentPrice;
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

	public void setTopBidderName(String topBidderName) {
		this.topBidderName = topBidderName;
	}

	public String getTopBidderName() {
		return topBidderName;
	}
}
