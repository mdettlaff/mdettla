package mdettla.englishauction.ontology;

import jade.content.Predicate;
import jade.content.onto.annotations.Slot;

public class BiddingPrice implements Predicate {

	private Integer price;

	public void setPrice(Integer price) {
		this.price = price;
	}

	@Slot(mandatory = true)
	public Integer getPrice() {
		return price;
	}
}
