package mdettla.jadex.pennyauctions;

import jade.core.AID;
import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;

public class AuctionSite extends Agent {
	private static final long serialVersionUID = 1L;

	@Override
	protected void setup() {
		Behaviour requestTranslation = new WakerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				System.out.println(myAgent.getName() +
						": wysyłam zapytanie o tłumaczenie");
				ACLMessage msg = new ACLMessage(ACLMessage.REQUEST);
				msg.addReceiver(new AID("translator", AID.ISLOCALNAME));
				msg.setContent("Hund");
				send(msg);
			}
		};

		addBehaviour(requestTranslation);
	}
}
