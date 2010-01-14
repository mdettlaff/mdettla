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
import jade.content.lang.Codec.CodecException;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.content.onto.OntologyException;
import jade.content.onto.UngroundedException;
import jade.content.onto.basic.Action;
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

	/**
	 * Co ile milisekund wysyłać wezwanie do licytowania.
	 */
	private static final int INTERVAL = 1000;

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
	 * O ile zwiększać cenę po zaakceptowaniu oferty.
	 */
	private static int incrementPriceBy = 5;

	private boolean isAuctionActive;

	/**
	 * Agent, który przedstawił do tej pory najlepszą ofertę.
	 */
	private AID topBidder;
	private String topBidderName;

	private int bidCount = 1;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getLocalName());

		askingPrice = Integer.valueOf(getArguments()[0].toString());
		reservationPrice = Integer.valueOf(getArguments()[1].toString());
		incrementPriceBy = Integer.valueOf(getArguments()[2].toString());
		currentPrice = askingPrice;

		getContentManager().registerLanguage(codec);
		getContentManager().registerOntology(ontology);

		final Behaviour checkBids = new TickerBehaviour(this, INTERVAL) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				if (isAuctionActive) {
					MessageTemplate mt = MessageTemplate.and(
							MessageTemplate.and(
								MessageTemplate.and(
									MessageTemplate.MatchLanguage(codec.getName()),
									MessageTemplate.MatchOntology(ontology.getName())),
								MessageTemplate.MatchPerformative(ACLMessage.PROPOSE)),
							MessageTemplate.MatchInReplyTo(bidCount + ""));
					ACLMessage msg = receive(mt);
					if (msg != null) {
						currentPrice += incrementPriceBy;
						handleBid(msg);
					} else if (isAuctionActive) { // brak oferty kupna
						informEndOfAuction();
						isAuctionActive = false;
						if (topBidder != null) {
							requestWinner();
						}
					}
				}
			}
		};

		Behaviour informStartOfAuction = new WakerBehaviour(this, INTERVAL) {
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

				isAuctionActive = true;
			}
		};
		addBehaviour(informStartOfAuction);

		Behaviour firstCFP = new WakerBehaviour(this, 2 * INTERVAL) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				sendCFPToBuyers();
			}
		};
		addBehaviour(firstCFP);

		Behaviour startCheckingMessages = new WakerBehaviour(this, 3 * INTERVAL) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				addBehaviour(checkBids);
			}
		};
		addBehaviour(startCheckingMessages);
	}

	/**
	 * Wezwanie do licytowania, do wszystkich kupujących.
	 */
	private void sendCFPToBuyers() {
		int priceToPropose = currentPrice + incrementPriceBy;
		System.out.println(getLocalName() +
				": wysyłam informację, że proponowana cena to " +
				priceToPropose);
		ACLMessage msg = new ACLMessage(ACLMessage.CFP);
		for (AID buyer : getBuyers()) {
			msg.addReceiver(buyer);
		}
		msg.setLanguage(codec.getName());
		msg.setOntology(ontology.getName());
		msg.setReplyWith(bidCount + "");
		BiddingPrice price = new BiddingPrice();
		price.setPrice(priceToPropose);
		try {
			getContentManager().fillContent(msg, price);
		} catch (Exception e) {
			e.printStackTrace();
		}
		send(msg);
	}

	private void sendConfirmTo(AID buyer) {
		ACLMessage msg = new ACLMessage(ACLMessage.CONFIRM);
		msg.addReceiver(buyer);
		send(msg);
	}

	private void sendDisconfirm() {
		ACLMessage msg = new ACLMessage(ACLMessage.DISCONFIRM);
		for (AID buyer : getBuyers()) {
			if (!buyer.equals(topBidder)) {
				msg.addReceiver(buyer);
			}
		}
		send(msg);
	}

	private void handleBid(ACLMessage msg) {
		try {
			ContentElement ce = null;
			ce = getContentManager().extractContent(msg);
			if (ce instanceof Action) {
				Action raiseBiddingPrice = (Action)ce;
				Bid bid = (Bid)raiseBiddingPrice.getAction();
				if (bid.getAbleToPay()) {
					topBidder = msg.getSender();
					topBidderName = bid.getBidderName();
					bidCount++;
					System.out.println(getLocalName()
							+ ": zaakceptowałem ofertę od: "
							+ bid.getBidderName());
					sendConfirmTo(topBidder);
					sendDisconfirm();
					sendCFPToBuyers();
				}
			}
		} catch (CodecException e) {
			e.printStackTrace();
		} catch (UngroundedException e) {
			e.printStackTrace();
		} catch (OntologyException e) {
			e.printStackTrace();
		}
	}

	private void informEndOfAuction() {
		ACLMessage msg = new ACLMessage(ACLMessage.INFORM);
		for (AID buyer : getBuyers()) {
			msg.addReceiver(buyer);
		}
		if (currentPrice >= reservationPrice) {
			msg.setContent("Koniec aukcji. Wygrał: " + topBidderName);
		} else {
			msg.setContent("Koniec aukcji. Przedmiot nie został sprzedany.");
		}
		System.out.println(getLocalName() + ": brak chętnych, ogłaszam koniec aukcji");
		send(msg);
	}

	private void requestWinner() {
		ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
		msg.setLanguage(codec.getName());
		msg.setOntology(ontology.getName());
		BiddingPrice finalPrice = new BiddingPrice();
		finalPrice.setPrice(currentPrice);
		try {
			getContentManager().fillContent(msg, finalPrice);
		} catch (Exception e) {
			e.printStackTrace();
		}
		msg.addReceiver(topBidder);
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
