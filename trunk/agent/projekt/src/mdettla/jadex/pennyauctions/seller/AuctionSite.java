package mdettla.jadex.pennyauctions.seller;

import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.ArrayList;
import java.util.List;

public class AuctionSite extends Agent {
	private static final long serialVersionUID = 1L;

	private List<User> subscribers = new ArrayList<User>();
	private List<PennyAuction> auctions = new ArrayList<PennyAuction>();
	private int netProfit = 0;

	@SuppressWarnings("serial")
	@Override
	protected void setup() {
		// rejestrujemy usługę w Yellow Pages (Directory Facilitator).
		DFAgentDescription dfd = new DFAgentDescription();
		dfd.setName(getAID());
		ServiceDescription sd = new ServiceDescription();
		sd.setType("penny_auction");
		sd.setName("licytacja typu penny auction");
		dfd.addServices(sd);
		try {
			DFService.register(this, dfd);
		} catch (FIPAException fe) {
			fe.printStackTrace();
		}

		Behaviour checkForNewRegistrations = new TickerBehaviour(this, 1000) {
			@Override
			public void onTick() {
				MessageTemplate mt = MessageTemplate.MatchPerformative(
						ACLMessage.SUBSCRIBE);
				ACLMessage msg = receive(mt);
				if (msg != null) {
					String username = msg.getContent();
					ACLMessage reply = msg.createReply();
					reply.setContent("");
					if (getUser(username) == null) {
						subscribers.add(new User(username, msg.getSender()));
						reply.setPerformative(ACLMessage.AGREE);
						System.out.println(myAgent.getName() +
						": agent zarejestrował się na stronie aukcyjnej");
					} else {
						reply.setPerformative(ACLMessage.REFUSE);
					}
					send(reply);
				}
			}
		};
		addBehaviour(checkForNewRegistrations);

		Behaviour checkForBids = new TickerBehaviour(this, 100) {
			@Override
			public void onTick() {
				MessageTemplate mt = MessageTemplate.MatchPerformative(
						ACLMessage.REQUEST);
				ACLMessage msg = receive(mt);
				// jeśli otrzymaliśmy podbicie
				if (msg != null && msg.getContent().startsWith("bid")) {
					boolean isBidAccepted = true;
					System.out.println(myAgent.getName() + ": otrzymałem podbicie");
					Integer auctionId = Integer.valueOf(msg.getContent().split(" ")[1]);
					String username = msg.getContent().split(" ")[2];
					User user = getUser(username);
					PennyAuction auction = getAuction(auctionId);
					if (user != null && auction != null
							&& user.getBidsLeft() > 0 && auction.isActive()) {
						auction.makeBid(user);
						user.setBidsLeft(user.getBidsLeft() - 1);
						isBidAccepted = true;
					} else {
						isBidAccepted = false;
					}

					// wysyłamy potwierdzenie (lub odrzucenie propozycji)
					ACLMessage reply = msg.createReply();
					reply.setContent(auction.getId().toString());
					if (isBidAccepted) {
						reply.setPerformative(ACLMessage.CONFIRM);
						reply.setContent("confirm_bid");
					} else {
						reply.setPerformative(ACLMessage.DISCONFIRM);
					}
					send(reply);
				} else if (msg != null && msg.getContent().startsWith("buy_bids")) {
					// jeśli otrzymaliśmy prośbę o zakup podbić
					System.out.println(myAgent.getName() + ": otrzymałem prośbę o kupno podbić");
					Integer bidsCount = Integer.valueOf(msg.getContent().split(" ")[1]);
					String username = msg.getContent().split(" ")[2];
					User user = getUser(username);
					if (user != null && bidsCount > 0) {
						user.buyBids(bidsCount);
						System.out.println(myAgent.getName() +
						": dodałem podbicia użytkownikowi");
					}
				}
			}
		};
		addBehaviour(checkForBids);

		startAuction(ProductsDatabase.getProduct(1), 11995, 5);
	}

	@SuppressWarnings("serial")
	private void startAuction(
			final Product product, final int initialPrice, final int timeLeft) {

		final PennyAuction newAuction = new PennyAuction(product, initialPrice, timeLeft);
		auctions.add(newAuction);

		Behaviour runAuction = new TickerBehaviour(this, 1000) {
			private PennyAuction auction = newAuction;
			@Override
			public void onTick() {
				if (auction.isActive() && subscribers.size() > 0) {
					ACLMessage msg = new ACLMessage(ACLMessage.CFP);
					for (User subscriber : subscribers) {
						msg.addReceiver(subscriber.getAID());
					}
					StringBuffer content = new StringBuffer();
					content.append("auction_state");
					content.append(" " + auction.getId());
					content.append(" " + auction.getProduct().getId());
					content.append(" " + auction.getCurrentPrice());
					content.append(" " + auction.getTimeLeft());
					String topBidder;
					if (auction.getTopBidder() != null) {
						topBidder = auction.getTopBidder().getName();
					} else {
						topBidder = "none";
					}
					content.append(" " + topBidder);
					msg.setContent(content.toString());
					send(msg);
					System.out.println(myAgent.getName() +
					": wysyłam wiadomość o stanie aukcji");
					auction.setTimeLeft(auction.getTimeLeft() - 1);
					if (auction.getTimeLeft() < 0) {
						auction.setActive(false);
						System.out.println("Koniec aukcji: " + auction);
					}
				}
			}
		};
		addBehaviour(runAuction);
	}

	private User getUser(String username) {
		for (User user : subscribers) {
			if (user.getName().equals(username)) {
				return user;
			}
		}
		return null;
	}

	private PennyAuction getAuction(Integer auctionId) {
		for (PennyAuction auction : auctions) {
			if (auction.getId().equals(auctionId)) {
				return auction;
			}
		}
		return null;
	}
}
