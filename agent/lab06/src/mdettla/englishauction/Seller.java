package mdettla.englishauction;

import mdettla.englishauction.ontology.EnglishAuctionOntology;
import mdettla.englishauction.ontology.BiddingPrice;
import mdettla.englishauction.ontology.Bid;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import jade.content.ContentElement;
import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;

public class Seller extends Agent {
	private static final long serialVersionUID = 1L;

	private Codec codec = new SLCodec();
	private Ontology ontology = EnglishAuctionOntology.getInstance();

	/**
	 * Cena wywoławcza.
	 */
	private int askingPrice;
	/**
	 * Minimalna cena, za jaką może zostać sprzedany przedmiot aukcji.
	 */
	private int reservationPrice;
	/**
	 * Aktualna cena przedmiotu.
	 */
	private int currentPrice;

	/**
	 * Agent, który przedstawił do tej pory najlepszą ofertę.
	 */
	private AID topBidder;
	private String topBidderName;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getLocalName());

		askingPrice = Integer.valueOf(getArguments()[0].toString());
		reservationPrice = Integer.valueOf(getArguments()[1].toString());
		currentPrice = askingPrice;

		getContentManager().registerLanguage(codec);
		getContentManager().registerOntology(ontology);

		final Behaviour checkBids = new TickerBehaviour(this, 750) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				MessageTemplate mt = MessageTemplate.and(MessageTemplate.and(
						MessageTemplate.MatchLanguage(codec.getName()),
						MessageTemplate.MatchOntology(ontology.getName())),
						MessageTemplate.MatchPerformative(ACLMessage.PROPOSE));
				ACLMessage msg = receive(mt);
				if (msg != null) {
					try {
						ContentElement ce = null;
						ce = getContentManager().extractContent(msg);
						if (ce instanceof Bid) {
							Bid bid = (Bid)ce;
							if (bid.getAbleToPay() && bid.getPrice() > currentPrice) {
								currentPrice = bid.getPrice();
								topBidder = msg.getSender();
								topBidderName = bid.getBidderName();
								System.out.println(myAgent.getLocalName() +
										": zaakceptowałem ofertę od: " + bid.getBidderName());
								System.out.println(getLocalName() +
										": wysyłam informację, że aktualna cena to " +
										currentPrice);
								sendCFPToBuyers();
							}
						}
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				// po uwzględnieniu najszybszej odpowiedzi, ignorujemy resztę
				while (msg != null) {
					msg = receive(mt);
				}
			}
		};

		Behaviour startAuction = new WakerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				// wysyłamy wiadomość o rozpoczęciu aukcji
				System.out.println(myAgent.getLocalName() +
						": wysyłam wiadomość o rozpoczęciu aukcji");
				ACLMessage startOfAuction = new ACLMessage(ACLMessage.INFORM);
				startOfAuction.setProtocol(
						FIPANames.InteractionProtocol.FIPA_ENGLISH_AUCTION);
				for (AID buyer : getBuyers()) {
					startOfAuction.addReceiver(buyer);
				}
				send(startOfAuction);

				sendCFPToBuyers();
				addBehaviour(checkBids);
			}
		};

		addBehaviour(startAuction);
	}

	/**
	 * Wezwanie do licytowania, do wszystkich kupujących.
	 */
	private void sendCFPToBuyers() {
		ACLMessage msg = new ACLMessage(ACLMessage.CFP);
		for (AID buyer : getBuyers()) {
			msg.addReceiver(buyer);
		}
		msg.setLanguage(codec.getName());
		msg.setOntology(ontology.getName());
		BiddingPrice price = new BiddingPrice();
		price.setPrice(currentPrice);
		try {
			getContentManager().fillContent(msg, price);
		} catch (Exception e) {
			e.printStackTrace();
		}
		send(msg);
	}

	private List<AID> getBuyers() {
		DFAgentDescription template = new DFAgentDescription();
		ServiceDescription sd = new ServiceDescription();
		sd.setType("bidding");
		template.addServices(sd);
		List<AID> buyers = null;
		try {
			DFAgentDescription[] result =
				DFService.search(this, template);
			buyers = new ArrayList<AID>(result.length);
			for (int i = 0; i < result.length; ++i) {
				buyers.add(result[i].getName());
			}
		} catch (FIPAException fe) {
			fe.printStackTrace();
		}
		return buyers;
	}

	protected void takeDown() {
		super.takeDown();
	}
}
