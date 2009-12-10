package mdettla.englishauction;

import mdettla.englishauction.ontology.EnglishAuctionOntology;
import mdettla.englishauction.ontology.BiddingPrice;

import java.util.Random;

import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.content.ContentElement;
import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;

public class Buyer extends Agent {
	private static final long serialVersionUID = 1L;

	private Codec codec = new SLCodec();
	private Ontology ontology = EnglishAuctionOntology.getInstance();

	/**
	 * Maksymalna cena, jaką agent kupujący jest w stanie zapłacić.
	 */
	private int maxPrice;
	private AID seller;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getLocalName());

		maxPrice = Integer.valueOf(getArguments()[0].toString());

		getContentManager().registerLanguage(codec);
		getContentManager().registerOntology(ontology);

		// rejestrujemy usługę w Yellow Pages (Directory Facilitator).
		DFAgentDescription dfd = new DFAgentDescription();
		dfd.setName(getAID());
		ServiceDescription sd = new ServiceDescription();
		sd.setType("bidding");
		sd.setName("licytowanie na English Auction");
		dfd.addServices(sd);
		try {
			DFService.register(this, dfd);
		} catch (FIPAException fe) {
			fe.printStackTrace();
		}

		Behaviour checkMessages = new TickerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				ACLMessage msg = receive();
				if (msg != null) {
					switch (msg.getPerformative()) {
						case ACLMessage.INFORM:
							if (FIPANames.InteractionProtocol.
									FIPA_ENGLISH_AUCTION.equals(
									msg.getProtocol())) {
								System.out.println(myAgent.getLocalName() +
										": dowiedziałem się o " +
										"rozpoczęciu aukcji");
								seller = msg.getSender();
							}
							break;
						case ACLMessage.CFP:
							if (msg.getSender().equals(seller)) {
								// TODO weryfikacja kodeka i ontologii za
								// pomocą MessageTemplate
								try {
									ContentElement ce = null;
									ce = getContentManager().
										extractContent(msg);
									if (ce instanceof BiddingPrice) {
										BiddingPrice price = (BiddingPrice)ce;
										System.out.println(
												myAgent.getLocalName() +
												": dowiedziałem się, że " +
												"nowa cena to " +
												price.getPrice());
										ACLMessage bidMsg = new ACLMessage(
												ACLMessage.PROPOSE);
										bidMsg.addReceiver(msg.getSender());
										send(bidMsg);
									}
								} catch (Exception e) {
									e.printStackTrace();
								}
							}
							break;
					}
				}
			}
		};

		addBehaviour(checkMessages);
	}

	@Override
	protected void takeDown() {
		// wyrejestrowujemy się z Yellow Pages
		try {
			DFService.deregister(this);
		} catch (FIPAException fe) {
			fe.printStackTrace();
		}
		super.takeDown();
	}
}
