import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.lang.acl.ACLMessage;

public class Cultist extends Agent {
	private static final long serialVersionUID = 1L;

	@Override
	protected void setup() {
		System.out.println("Pojawia się kultysta.");

		Behaviour lookAround = new TickerBehaviour(this, 2000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				System.out.println("Kultysta się rozgląda.");
				ACLMessage msg = receive();
				if (msg == null) {
					System.out.println("Kultysta nie widzi nic podejrzanego.");
					return; // nie ma nowych wiadomości
				}
				String content = msg.getContent();
				if (content.contains("jedzonko")) {
					System.out.println("Kultysta woła: " +
							"\"O, nie! Cthulhu chce mnie zjeść!\"");
				} else {
					System.out.println("Kultysta słyszy tajemniczy dźwięk.");
				}
			}
		};

		addBehaviour(lookAround);
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
