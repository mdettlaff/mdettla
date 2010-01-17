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

import mdettla.jadex.pennyauctions.util.Utils;

public class AuctionSite extends Agent {
	private static final long serialVersionUID = 1L;

	private List<User> subscribers = new ArrayList<User>();
	private List<PennyAuction> auctions = new ArrayList<PennyAuction>();
	private int netOutcomings = 0;
	private int netIncomings = 0;

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

		Behaviour checkForNewRegistrations = new TickerBehaviour(this, 500) {
			@Override
			public void onTick() {
				MessageTemplate mt = MessageTemplate.MatchPerformative(
						ACLMessage.SUBSCRIBE);
				ACLMessage msg = receive(mt);
				if (msg != null) {
					String username = msg.getContent();
					ACLMessage reply = msg.createReply();
					StringBuffer content = new StringBuffer("registered");
					content.append(" " + PennyAuction.BID_PRICE);
					content.append(" " + PennyAuction.BIDS_IN_PACKAGE);
					reply.setContent(content.toString());
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

		Behaviour checkForBids = new TickerBehaviour(this, 50) {
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
					if (isBidAccepted) {
						reply.setPerformative(ACLMessage.CONFIRM);
						reply.setContent("confirm_bid " + auction.getId());
					} else {
						reply.setPerformative(ACLMessage.DISCONFIRM);
					}
					send(reply);
				} else if (msg != null && msg.getContent().startsWith("buy_bids")) {
					// jeśli otrzymaliśmy prośbę o zakup podbić
					System.out.println(myAgent.getName() + ": otrzymałem prośbę o kupno podbić");
					Integer bidPackagesCount = Integer.valueOf(msg.getContent().split(" ")[1]);
					String username = msg.getContent().split(" ")[2];
					User user = getUser(username);
					if (user != null && bidPackagesCount > 0) {
						user.buyBids(bidPackagesCount);
						netIncomings += bidPackagesCount * PennyAuction.BID_PRICE *
								PennyAuction.BIDS_IN_PACKAGE;
						System.out.println(myAgent.getName() +
						": dodałem podbicia użytkownikowi");
					}
				}
			}
		};
		addBehaviour(checkForBids);

		startAuction(ProductsDatabase.getProduct(1), 11990, 5);
	}

	@SuppressWarnings("serial")
	private void startAuction(
			final Product product, final int initialPrice, final int timeLeft) {

		final PennyAuction newAuction = new PennyAuction(product, initialPrice, timeLeft);
		auctions.add(newAuction);

		Behaviour runAuction = new TickerBehaviour(this, 200) {
			private PennyAuction auction = newAuction;
			@Override
			public void onTick() {
				if (auction.isActive() && subscribers.size() > 0) {
					ACLMessage msg = new ACLMessage(ACLMessage.CFP);
					for (User subscriber : subscribers) {
						msg.addReceiver(subscriber.getAID());
					}
					StringBuffer content = new StringBuffer();
					String topBidder;
					if (auction.getTopBidder() != null) {
						topBidder = auction.getTopBidder().getName();
					} else {
						topBidder = "none";
					}
					if (auction.getTimeLeft() > 0) {
						// informacja o trwaniu aukcji
						content.append("auction_running");
						content.append(" " + auction.getId());
						content.append(" " + auction.getProduct().getId());
						content.append(" " + auction.getCurrentPrice());
						content.append(" " + topBidder);
						content.append(" " + auction.getTimeLeft());
					} else {
						// informacja o zakończeniu aukcji
						auction.setActive(false);
						msg.setPerformative(ACLMessage.INFORM);
						content.append("auction_end");
						content.append(" " + auction.getId());
						content.append(" " + auction.getProduct().getId());
						content.append(" " + auction.getCurrentPrice());
						content.append(" " + topBidder);
						if (auction.getTopBidder() != null) {
							auction.getTopBidder().addMoneySpent(
									auction.getCurrentPrice());
							netIncomings += auction.getCurrentPrice();
							netOutcomings += auction.getProduct().getRetailPrice();
						}
						System.out.println("Koniec aukcji: " + auction);
						System.out.println("PRZYCHODY: " +
								Utils.formatPrice(netIncomings) + " zł");
						System.out.println("WYDATKI: " +
								Utils.formatPrice(netOutcomings) + " zł");
						System.out.println("SUMARYCZNY ZYSK: " +
								Utils.formatPrice(netIncomings - netOutcomings) +
								" zł");
					}
					msg.setContent(content.toString());
					send(msg);
					System.out.println(myAgent.getName() +
							": wysyłam wiadomość o stanie aukcji " +
							"(sekund " + auction.getTimeLeft() + ")");
					auction.setTimeLeft(auction.getTimeLeft() - 1);
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
