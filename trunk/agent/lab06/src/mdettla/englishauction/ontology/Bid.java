package mdettla.englishauction.ontology;

import jade.content.Concept;
import jade.content.onto.annotations.Slot;

public class Bid implements Concept {

	private Integer price;
	private String bidderName;

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
