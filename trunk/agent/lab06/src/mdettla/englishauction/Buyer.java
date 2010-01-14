package mdettla.englishauction;

import mdettla.englishauction.ontology.EnglishAuctionOntology;
import mdettla.englishauction.ontology.BiddingPrice;
import mdettla.englishauction.ontology.Bid;

import java.util.Random;

import jade.content.lang.Codec;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.Ontology;
import jade.content.onto.basic.Action;
import jade.content.ContentElement;
import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.domain.FIPAException;
import jade.domain.FIPANames;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;

public class Buyer extends Agent {
	private static final long serialVersionUID = 1L;

	private static final Random random = new Random();

	private Codec codec = new SLCodec();
	private Ontology ontology = EnglishAuctionOntology.getInstance();

	/**
	 * Maksymalna cena, jaką agent kupujący jest w stanie zapłacić.
	 */
	private int maxPrice;
	private AID seller;
	private boolean isPreviousBidMine = false;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getLocalName());

		maxPrice = Integer.valueOf(getArguments()[0].toString());
		int checkMsgInterval = Integer.valueOf(getArguments()[1].toString());

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

		Behaviour checkMessages =
			new TickerBehaviour(this, checkMsgInterval) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				ACLMessage msg = receive();
				if (msg != null) {
					switch (msg.getPerformative()) {
						case ACLMessage.INFORM:
							handleInform(msg);
							break;
						case ACLMessage.CFP:
							handleCFP(msg);
							break;
						case ACLMessage.CONFIRM:
							isPreviousBidMine = true;
							break;
						case ACLMessage.DISCONFIRM:
							isPreviousBidMine = false;
							break;
						case ACLMessage.REQUEST:
							handleRequest(msg);
					}
				}
			}
		};

		addBehaviour(checkMessages);
	}

	private void handleInform(ACLMessage msg) {
		if (FIPANames.InteractionProtocol.FIPA_ENGLISH_AUCTION.equals(
					msg.getProtocol()) && seller == null) {
			System.out.println(getLocalName() +
					": dowiedziałem się o rozpoczęciu aukcji");
			seller = msg.getSender();
		} else if (seller != null) {
			System.out.println(getLocalName() +
					": dowiedziałem się o zakończeniu aukcji (" +
					msg.getContent() + ")");
		}
	}

	private void handleCFP(ACLMessage msg) {
		MessageTemplate mt = MessageTemplate.and(
				MessageTemplate.MatchLanguage(codec.getName()),
				MessageTemplate.MatchOntology(ontology.getName()));
		if (seller != null && msg.getSender().equals(seller)
				&& mt.match(msg)) {
			try {
				ContentElement ce = null;
				ce = getContentManager().extractContent(msg);
				if (ce instanceof BiddingPrice) {
					BiddingPrice biddingPrice = (BiddingPrice)ce;
					makeBid(biddingPrice, msg.createReply(), msg.getSender());
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Wysyłamy ofertę do prowadzącego aukcję, jeśli odpowiadają nam warunki.
	 */
	private void makeBid(BiddingPrice biddingPrice, ACLMessage bidMsg, AID sender) {
		try {
			if (!isPreviousBidMine) {
				if (maxPrice >= biddingPrice.getPrice()) {
					bidMsg.setPerformative(ACLMessage.PROPOSE);
					Action raiseBiddingPrice = new Action();
					Bid bid = new Bid();
					bid.setAbleToPay(true);
					bid.setBidderName(getLocalName());
					raiseBiddingPrice.setAction(bid);
					raiseBiddingPrice.setActor(sender);
					getContentManager().fillContent(bidMsg, raiseBiddingPrice);
					send(bidMsg);
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void handleRequest(ACLMessage msg) {
		try {
			ContentElement ce = null;
			ce = getContentManager().extractContent(msg);
			if (ce instanceof BiddingPrice) {
				BiddingPrice finalPrice = (BiddingPrice)ce;
				System.out.println(getLocalName()
						+ ": dostałem wezwanie do kupna za "
						+ finalPrice.getPrice());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
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
