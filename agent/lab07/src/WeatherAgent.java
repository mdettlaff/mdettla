import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

import java.util.Random;

public class WeatherAgent extends Agent {

	@Override
	protected void setup() {
		System.out.println("WeatherAgent: setup");

		Behaviour receiveMessages = new CyclicBehaviour(this) {
			@Override
			public void action() {
				MessageTemplate queryIf =
					MessageTemplate.MatchPerformative(ACLMessage.QUERY_IF);
				ACLMessage msg = receive(queryIf);
				if (msg != null) {
					String content = msg.getContent();
					System.out.println("WeatherAgent: dostałem QUERY_IF");
					System.out.println("WeatherAgent: wysyłam INFORM");
					ACLMessage response = new ACLMessage(ACLMessage.INFORM);
					response.addReceiver(msg.getSender());
					response.setContent(String.valueOf(
								(new Random().nextInt() % 10) + 20));
					send(response);
				} else {
					block();
				}
			}
		};
		addBehaviour(receiveMessages);
	}
}
