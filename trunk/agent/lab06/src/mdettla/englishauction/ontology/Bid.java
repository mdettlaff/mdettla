package mdettla.englishauction.ontology;

import jade.content.Concept;
import jade.content.Predicate;
import jade.content.onto.annotations.Slot;

public class Bid implements Concept {

	private String bidderName;
	private Boolean ableToPay;

	public void setAbleToPay(Boolean ableToPay) {
		this.ableToPay = ableToPay;
	}

	public Boolean getAbleToPay() {
		return ableToPay;
	}

	public void setBidderName(String bidderName) {
		this.bidderName = bidderName;
	}

	public String getBidderName() {
		return bidderName;
	}
}
