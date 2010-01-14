package mdettla.englishauction.ontology;

import jade.content.Concept;
import jade.content.Predicate;
import jade.content.onto.annotations.Slot;

public class Bid implements Concept {

	private Boolean ableToPay;
	private Integer price;
	private String bidderName;

	public void setAbleToPay(Boolean ableToPay) {
		this.ableToPay = ableToPay;
	}

	@Slot(mandatory = true)
	public Boolean getAbleToPay() {
		return ableToPay;
	}

	public void setPrice(Integer price) {
		this.price = price;
	}

	@Slot(mandatory = true)
	public Integer getPrice() {
		return price;
	}

	public void setBidderName(String bidderName) {
		this.bidderName = bidderName;
	}

	public String getBidderName() {
		return bidderName;
	}
}
