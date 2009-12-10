package mdettla.englishauction.ontology;

import jade.content.onto.BeanOntology;
import jade.content.onto.Ontology;

public class EnglishAuctionOntology extends BeanOntology {

	private static final String ONTOLOGY_NAME = "Ontologia English Auction";

	private static Ontology instance =
		new EnglishAuctionOntology(ONTOLOGY_NAME);

	public static Ontology getInstance() {
		return instance;
	}

	private EnglishAuctionOntology(String name) {
		super(name);

		try {
			add(Bid.class);
			add(BiddingPrice.class);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
