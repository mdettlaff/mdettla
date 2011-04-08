import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;

public class PersonalAgent extends Agent {

	private Integer knownTemperature;

	@Override
	protected void setup() {
		System.out.println("PersonalAgent: setup");

		Behaviour receiveMessages = new CyclicBehaviour(this) {
			@Override
			public void action() {
				ACLMessage msg = receive();
				if (msg != null) {
					int performative = msg.getPerformative();
					switch (performative) {
						case ACLMessage.REQUEST:
							handleRequest(msg);
							break;
						case ACLMessage.QUERY_IF:
							handleQueryIf(msg);
							break;
						case ACLMessage.INFORM:
							handleInform(msg);
							break;
						default:
							throw new RuntimeException(
									"Unknown performative: " + performative);
					}
				} else {
					block();
				}
			}
		};
		addBehaviour(receiveMessages);
	}

	private void handleRequest(ACLMessage msg) {
		System.out.println("PersonalAgent: dostałem REQUEST");
		if (knownTemperature != null) {
			System.out.println("PersonalAgent: wysyłam INFORM");
			ACLMessage response = new ACLMessage(ACLMessage.INFORM);
			response.addReceiver(msg.getSender());
			response.setContent(String.valueOf(knownTemperature));
			send(response);
		}
	}

	private void handleQueryIf(ACLMessage msg) {
		System.out.println("PersonalAgent: dostałem QUERY_IF");
		if (knownTemperature != null) {
			System.out.println("PersonalAgent: wysyłam INFORM");
			ACLMessage response = new ACLMessage(ACLMessage.INFORM);
			response.addReceiver(msg.getSender());
			response.setContent(String.valueOf(knownTemperature));
			send(response);
		} else {
			String content = msg.getContent();
			System.out.println("PersonalAgent: wysyłam QUERY_IF");
			ACLMessage queryIfMsg = new ACLMessage(ACLMessage.QUERY_IF);
			queryIfMsg.addReceiver(new AID("weather", AID.ISLOCALNAME));
			send(queryIfMsg);
		}
	}

	private void handleInform(ACLMessage msg) {
		String content = msg.getContent();
		System.out.println("PersonalAgent: " +
				"dostałem INFORM, że temperatura jest " + content);
		knownTemperature = Integer.valueOf(content);
	}
}
