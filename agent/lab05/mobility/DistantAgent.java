import java.util.Iterator;

import jade.core.Agent;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.core.AID;
import jade.core.Location;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.content.lang.sl.SLCodec;
import jade.domain.FIPANames;
import jade.domain.FIPANames.ContentLanguage;
import jade.domain.FIPANames.InteractionProtocol;
import jade.domain.JADEAgentManagement.WhereIsAgentAction;
import jade.domain.mobility.MobilityOntology;

public class DistantAgent extends Agent {
	private static final long serialVersionUID = 1L;

	private ACLMessage prepareRequestToAMS(AID agent) {
		ACLMessage request = null;
		try {
			request = new ACLMessage(ACLMessage.REQUEST);
			request.addReceiver(getAMS());
			//request.setLanguage(FIPANames.ContentLanguage.FIPA_SL0);
			request.setLanguage(new SLCodec().getName());
			request.setOntology(MobilityOntology.NAME);
			request.setProtocol(FIPANames.InteractionProtocol.FIPA_REQUEST);
			Action act = new Action();
			act.setActor(getAMS());

			WhereIsAgentAction action = new WhereIsAgentAction();
			action.setAgentIdentifier(agent);
			act.setAction(action);

			getContentManager().fillContent(request, act);
		} catch (Exception e) {
			System.err.println("Tworzenie zapytania o przemieszczenie:");
			e.printStackTrace();
		}
		return request;
	}

	private Location parseAMSResponse(ACLMessage response) {
		Location loc = null;
		try {
			Result results =
				(Result)getContentManager().extractContent(response);
			Iterator it = results.getItems().iterator();
			if (it.hasNext()) {
				loc = (Location)it.next();
			}
		} catch (Exception e) {
			System.err.println("Parsowanie zapytania o przemieszczenie:");
			e.printStackTrace();
		}
		return loc;
	}

	@Override
	protected void setup() {
		System.out.println("Odlegly agent: setup");

		getContentManager().registerLanguage(new SLCodec());
		getContentManager().registerOntology(MobilityOntology.getInstance());

		Behaviour waker = new WakerBehaviour(this, 3000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onWake() {
				System.out.println("Wysyłam prośbę o podanie lokacji.");
				AID myAgent = new AID("ta", AID.ISLOCALNAME);
				ACLMessage msg = prepareRequestToAMS(myAgent);
				send(msg);
			}
		};

		Behaviour ticker = new TickerBehaviour(this, 4000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onTick() {
				System.out.println("Jestem sobie na innym kontenerze.");
				System.out.println("Sprawdzam odpowiedź od AMS...");
				ACLMessage response = receive();
				if (response != null) {
					Location location = parseAMSResponse(response);
					doMove(location);
				} else {
					System.err.println("AMS nie odpowiedział na " +
							"zapytanie o lokalizację.");
				}
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
