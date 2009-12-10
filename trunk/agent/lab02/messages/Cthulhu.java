import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;

public class Cthulhu extends Agent {
	private static final long serialVersionUID = 1L;

	@Override
	protected void setup() {
		System.out.println("Cthulhu budzi się!");

		Behaviour hungry = new TickerBehaviour(this, 7000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				System.out.println("Cthulhu jest głodny...");
				ACLMessage msg = new ACLMessage(ACLMessage.INFORM);
				msg.addReceiver(new AID("cultist", AID.ISLOCALNAME));
				msg.setContent("O, jedzonko!");
				send(msg);
			}
		};

		addBehaviour(hungry);
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
