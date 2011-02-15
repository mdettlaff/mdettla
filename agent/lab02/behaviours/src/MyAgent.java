import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;

public class MyAgent extends Agent {

	@Override
	protected void setup() {
		System.out.println("Mój agent: setup");

		Behaviour oneShot = new OneShotBehaviour(this) {
			@Override
			public void action() {
				System.out.println("Nazywam się " + getAID().getName());
			}
		};
		addBehaviour(oneShot);

		Behaviour waker = new WakerBehaviour(this, 4000) {
			@Override
			protected void onWake() {
				System.out.println("Cthulhu budzi sie ze snu!!!");
			}
		};
		addBehaviour(waker);

		Behaviour ticker = new TickerBehaviour(this, 5000) {
			private static final long serialVersionUID = 1L;
			@Override
			protected void onTick() {
				System.out.println("Cthulhu zjadł kultystę.");
			}
		};
		addBehaviour(ticker);
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
