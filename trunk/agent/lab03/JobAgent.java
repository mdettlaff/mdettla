import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import jade.core.Agent;
import jade.core.AID;
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.TickerBehaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;

public class JobAgent extends Agent {
	private static final long serialVersionUID = 1L;

	private static int receiversCount;

	@Override
	protected void setup() {
		System.out.println("Tworzy się agent " + getClass().getName());

		receiversCount = Integer.valueOf(getArguments()[0].toString());

		Behaviour informAboutJob = new WakerBehaviour(this, 1000) {
			private static final long serialVersionUID = 1L;

			@Override
			public void onWake() {
				System.out.println(myAgent.getClass().getName() +
						": wysyłam wiadomość o pracy do wykonania");
				ACLMessage msg = new ACLMessage(ACLMessage.CFP);
				for (String name : getReceiversNames()) {
					msg.addReceiver(new AID(name, AID.ISLOCALNAME));
				}
				send(msg);
			}

			private List<String> getReceiversNames() {
				List<String> receiversNames =
					new ArrayList<String>(receiversCount);
				for (int i = 1; i <= receiversCount; i++) {
					receiversNames.add("tempagent" + i);
				}
				return receiversNames;
			}
		};

		Behaviour checkReplies = new WakerBehaviour(this, 3000) {
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
				myAgent.addBehaviour(new MeasurmentReceiver(myAgent));
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
