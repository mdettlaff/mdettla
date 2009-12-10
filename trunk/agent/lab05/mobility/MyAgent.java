import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;

public class MyAgent extends Agent {
	private static final long serialVersionUID = 1L;

	@Override
	protected void setup() {
		System.out.println("Moj agent: setup");

		Behaviour waker = new WakerBehaviour(this, 4000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onWake() {
				System.out.println("Cthulhu budzi sie ze snu!!!");
			}
		};

		Behaviour ticker = new TickerBehaviour(this, 5000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				System.out.println("Cthulhu zjadl kultyste.");
			}
		};

		addBehaviour(waker);
		addBehaviour(ticker);
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
