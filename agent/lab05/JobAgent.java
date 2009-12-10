import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import jade.core.Agent;
import jade.core.AID;
import jade.core.Location;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.domain.FIPAException;
import jade.domain.DFService;
import jade.domain.FIPAAgentManagement.DFAgentDescription;
import jade.domain.FIPAAgentManagement.ServiceDescription;
import jade.content.onto.basic.Action;
import jade.content.onto.basic.Result;
import jade.content.lang.sl.SLCodec;
import jade.domain.FIPANames;
import jade.domain.FIPANames.ContentLanguage;
import jade.domain.FIPANames.InteractionProtocol;
import jade.domain.JADEAgentManagement.WhereIsAgentAction;
import jade.domain.mobility.MobilityOntology;

public class JobAgent extends Agent {
	private static final long serialVersionUID = 1L;

	private static int receiversCount;

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

	/**
	 * Przemieszcza agenta do kontenera, w którym znajduje się miernik o
	 * najlepszej ofercie.
	 */
	private void moveToContainerOf(AID bestTemperatureAgent) {
		System.out.println(getClass().getName() +
				": wysyłam prośbę o podanie lokacji miernika");
		ACLMessage msgLoc = prepareRequestToAMS(bestTemperatureAgent);
		send(msgLoc);
		System.out.println(getClass().getName() +
				": sprawdzam wiadomość o lokacji miernika");
		ACLMessage response = blockingReceive();
		if (response != null) {
			Location location = parseAMSResponse(response);
			System.out.println(getClass().getName() +
					": przemieszczam się do lokacji miernika");
			doMove(location);
		} else {
			System.err.println("AMS nie odpowiedział na " +
					"zapytanie o lokalizację.");
		}
	}

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getClass().getName());

		receiversCount = Integer.valueOf(getArguments()[0].toString());

		getContentManager().registerLanguage(new SLCodec());
		getContentManager().registerOntology(MobilityOntology.getInstance());

		Behaviour informAboutJob = new WakerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				System.out.println(myAgent.getClass().getName() +
						": wysyłam wiadomość o pracy do wykonania");
				ACLMessage msg = new ACLMessage(ACLMessage.CFP);
				for (AID tempAgent : getTemperatureAgents()) {
					msg.addReceiver(tempAgent);
				}
				send(msg);
			}

			private List<AID> getTemperatureAgents() {
				DFAgentDescription template = new DFAgentDescription();
				ServiceDescription sd = new ServiceDescription();
				sd.setType("temperature-measurment");
				template.addServices(sd);
				List<AID> tempAgents = null;
				try {
					DFAgentDescription[] result =
						DFService.search(myAgent, template);
					tempAgents = new ArrayList<AID>(result.length);
					for (int i = 0; i < result.length; ++i) {
						tempAgents.add(result[i].getName());
					}
				} catch (FIPAException fe) {
					fe.printStackTrace();
				}
				return tempAgents;
			}
		};

		Behaviour checkReplies = new WakerBehaviour(this, 5000) {
			private static final long serialVersionUID = 1L;

			@Override
			protected void onWake() {
				System.out.println(myAgent.getClass().getName() +
						": sprawdzam odpowiedzi... ");
				Map<AID, Integer> proposals = new HashMap<AID, Integer>();
				MessageTemplate accept = MessageTemplate.MatchPerformative(
						ACLMessage.ACCEPT_PROPOSAL);
				ACLMessage msg;
				while ((msg = receive(accept)) != null) {
					String content = msg.getContent();
					int proposedPrice = Integer.valueOf(content);
					proposals.put(msg.getSender(), proposedPrice);
				}
				if (!proposals.isEmpty()) {
					Iterator<Map.Entry<AID, Integer>> it =
						proposals.entrySet().iterator();
					// szukamy najniższej ceny
					Map.Entry<AID, Integer> bestProposal = it.next();
					while (it.hasNext()) {
						Map.Entry<AID, Integer> entry = it.next();
						if (entry.getValue() < bestProposal.getValue()) {
							bestProposal = entry;
						}
					}

					System.out.println(myAgent.getClass().getName() +
							": z propozycji " + proposals.values() +
							" akceptuję " + bestProposal.getValue());
					ACLMessage agree = new ACLMessage(ACLMessage.AGREE);
					agree.addReceiver(bestProposal.getKey());
					send(agree);

					moveToContainerOf(bestProposal.getKey());
					// sprawdzamy okresowo informacje o aktualnej temperaturze
					myAgent.addBehaviour(new MeasurmentReceiver(myAgent));
				} else {
					System.out.println(myAgent.getClass().getName() +
							": nikt nie odpowiedział na zapytanie");
				}
			}
		};

		addBehaviour(informAboutJob);
		addBehaviour(checkReplies);
	}


	private class MeasurmentReceiver extends TickerBehaviour {

		public MeasurmentReceiver(Agent agent) {
			super(agent, 800);
		}

		@Override
		protected void onTick() {
			MessageTemplate inf = MessageTemplate.MatchPerformative(
					ACLMessage.INFORM);
			ACLMessage msg = receive(inf);
			if (msg != null) {
				System.out.println(myAgent.getClass().getName() +
						": dostałem informację, że temperatura wynosi " +
						msg.getContent());
			}
		}
	}

	@Override
	protected void takeDown() {
		super.takeDown();
	}
}
