package pl.edu.agh.jadex;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;

/**
 * Agent zlecający agentowi-tłumaczowi przetłumaczenie słów.
 */
public class TranslationClient extends Agent {
	private static final long serialVersionUID = 1L;

	@SuppressWarnings("serial")
	@Override
	protected void setup() {
		final AID TRANSLATOR = new AID("translator", AID.ISLOCALNAME);

		addBehaviour(new WakerBehaviour(this, 1000) {
			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate Katze");
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 2000) {
			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate Hund");
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 3000) {
			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o dodanie słowa do słownika");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("add Hund dog");
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 4000) {
			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate Hund");
				send(msg);
			}
		});
	}
}
