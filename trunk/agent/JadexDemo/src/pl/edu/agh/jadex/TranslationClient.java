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

		addBehaviour(new WakerBehaviour(this, 500) {
			@Override
			public void onWake() {
				String word = "dog";
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie: " + word);
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate " + word);
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 1000) {
			@Override
			public void onWake() {
				String word = "cat";
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie: " + word);
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate " + word);
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 1500) {
			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o dodanie słowa do słownika " +
						"cat Katze");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("add cat Katze");
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 2000) {
			@Override
			public void onWake() {
				String word = "cat";
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie: " + word);
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate " + word);
				send(msg);
			}
		});

		addBehaviour(new WakerBehaviour(this, 2500) {
			@Override
			public void onWake() {
				String word = "dog";
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie: " + word);
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(TRANSLATOR);
				msg.setContent("translate " + word);
				send(msg);
			}
		});
	}
}
