import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

public class UserAgent extends Agent {

	@Override
	protected void setup() {
		System.out.println("UserAgent: setup");

		Behaviour sendingRequest = new TickerBehaviour(this, 2000) {

			@Override
			protected void onTick() {
				System.out.println("UserAgent: wysyłam QUERY_IF");
				ACLMessage msg = new ACLMessage(ACLMessage.QUERY_IF);
				msg.addReceiver(new AID("personal", AID.ISLOCALNAME));
				send(msg);
			}
		};
		addBehaviour(sendingRequest);

		Behaviour receiveMessages = new CyclicBehaviour(this) {
			@Override
			public void action() {
				MessageTemplate inform =
					MessageTemplate.MatchPerformative(ACLMessage.INFORM);
				ACLMessage msg = receive(inform);
				if (msg != null) {
					String content = msg.getContent();
					System.out.println("UserAgent: dostałem INFORM, " +
							"że temperatura wynosi: " + content);
				} else {
					block();
				}
			}
		};
		addBehaviour(receiveMessages);
	}
}
