package mdettla.jadex.pennyauctions.seller;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.domain.DFService;
import jade.domain.FIPAException;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.lang.acl.ACLMessage;

import java.util.ArrayList;
import java.util.List;

public class AuctionSite extends Agent {
	private static final long serialVersionUID = 1L;

	private List<AID> subscribers = new ArrayList<AID>();
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

		Behaviour checkForNewRegistrations = new TickerBehaviour(this, 1500) {
			@Override
			public void onTick() {
				ACLMessage msg = receive();
				if (msg != null) {
					if (msg.getContent().startsWith("register")) {
						subscribers.add(msg.getSender());
						System.out.println(myAgent.getName() +
						": agent zarejestrował się na stronie aukcyjnej");
					}
				}
			}
		};
		addBehaviour(checkForNewRegistrations);

		startAuction(ProductsDatabase.getProducts().get(0), 100, 10);
	}

	@SuppressWarnings("serial")
	private void startAuction(
			final Product product, final int initialPrice, final int timeLeft) {

		Behaviour runAuction = new TickerBehaviour(this, 1000) {
			private PennyAuction auction =
				new PennyAuction(product, initialPrice, timeLeft);
			@Override
			public void onTick() {
				if (subscribers != null && subscribers.size() > 0) {
					ACLMessage msg = new ACLMessage(ACLMessage.INFORM);
					for (AID subscriber : subscribers) {
						msg.addReceiver(subscriber);
					}
					msg.setContent("auction_state wibble");
					send(msg);
					System.out.println(myAgent.getName() +
					": wysyłam wiadomość o stanie aucji");
					auction.setTimeLeft(auction.getTimeLeft() - 1);
				}
			}
		};
		addBehaviour(runAuction);
	}
}
