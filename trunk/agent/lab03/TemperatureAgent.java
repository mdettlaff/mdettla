import java.util.Random;

import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;

public class TemperatureAgent extends Agent {
	private static final long serialVersionUID = 1L;
	/**
	 * Cena za wykonanie pracy.
	 */
	private int price;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getClass().getName());

		price = Integer.valueOf(getArguments()[0].toString());

		Behaviour checkMessages = new TickerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				ACLMessage msg = receive();
				if (msg != null) {
					switch (msg.getPerformative()) {
						case ACLMessage.CFP:
							ACLMessage response =
								new ACLMessage(ACLMessage.ACCEPT_PROPOSAL);
							response.setContent("" + price);
							response.addReceiver(msg.getSender());
							send(response);
							break;
						case ACLMessage.AGREE:
							myAgent.addBehaviour(new MeasurmentSender(
										myAgent, msg.getSender()));
							break;
					}
				}
			}
		};

		addBehaviour(checkMessages);
	}

	private class MeasurmentSender extends TickerBehaviour {

		private Random random = new Random();
		private AID receiver;

		public MeasurmentSender(Agent agent, AID receiver) {
			super(agent, 1000);
			this.receiver = receiver;
		}

		@Override
		protected void onTick() {
			ACLMessage msg = new ACLMessage(ACLMessage.INFORM);
			msg.setContent((15 + random.nextInt(10)) +
					" za cenę: " + price);
			msg.addReceiver(receiver);
			send(msg);
		}
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
