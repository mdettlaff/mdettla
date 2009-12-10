package jade.misc;

import jade.content.lang.sl.SLCodec;
import jade.core.Agent;
import jade.core.AID;
import jade.core.ServiceException;
import jade.core.behaviours.CyclicBehaviour;
import jade.core.behaviours.FSMBehaviour;
import jade.core.behaviours.OneShotBehaviour;
import jade.core.behaviours.ParallelBehaviour;
import jade.core.behaviours.WakerBehaviour;
import jade.core.messaging.TopicManagementService;
import jade.core.messaging.TopicManagementHelper;
import jade.domain.FIPANames;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.util.leap.Serializable;

/**
 * This class can be used when a group of agents need to coordinate to elect one leader.
 * Each agent in the group is expected to use one LeadershipManager object.
 * The leader election procedure is based on the bully algorithm 
 */
public class LeadershipManager implements Serializable {
	public static final long DEFAULT_PROPOSAL_RESPONSE_TIMEOUT = 10000;
	public static final long DEFAULT_PROPOSAL_RETRY_INTERVAL = 10000;
	
	public static final String LEADER_ELECTION_TOPIC = "leader-election";
	
	public static final int IDLE = -1;
	public static final int LEADERSHIP_KNOWN = 0;
	public static final int LEADERSHIP_ACQUIRED = 1;
	public static final int ACQUIRING_LEADERSHIP = 2;
	public static final int WAITING = 3;
	
	private static final String TRY_ACQUIRE_STATE = "Try-Acquire";
	private static final String WAIT_STATE = "Wait";
	private static final String DUMMY_FINAL_STATE = "Dummy-Final";

	private Agent myAgent;
	private int status = IDLE;
	private AID leader;
	private AID leaderElectionTopic;
	
	private long upTime;
	
	private long proposalResponseTimeout = DEFAULT_PROPOSAL_RESPONSE_TIMEOUT;
	private long proposalRetryInterval = DEFAULT_PROPOSAL_RETRY_INTERVAL;
	
	/**
	 * Bind this <code>LeadershipManager</code> to a given agent, initialize configuration properties,
	 * register to receive messages about the leader-election topic, starts the behaviour listening 
	 * for such messages and initialize the upTime;
	 * @param a The Agent to bind to
	 */
	public void init(Agent a) throws ServiceException {
		myAgent  = a;
		
		if (myAgent.getContentManager().lookupLanguage(FIPANames.ContentLanguage.FIPA_SL) == null) {
			myAgent.getContentManager().registerLanguage(new SLCodec(), FIPANames.ContentLanguage.FIPA_SL);
		}
		myAgent.getContentManager().registerOntology(LeaderElectionOntology.getInstance());
		
		TopicManagementHelper topicHelper = (TopicManagementHelper) myAgent.getHelper(TopicManagementService.NAME);
		leaderElectionTopic = topicHelper.createTopic(LEADER_ELECTION_TOPIC);
		topicHelper.register(leaderElectionTopic);
		
		myAgent.addBehaviour(new LeaderElectionResponder(myAgent));
		upTime = System.currentTimeMillis();
	}
	
	public boolean isLeader() {
		return status == LEADERSHIP_ACQUIRED;
	}
	
	public AID getLeader() {
		return leader;
	}
	
	/**
	 * Activate the leader election procedure
	 */
	public void updateLeadership() {
		leader = null;
		status = ACQUIRING_LEADERSHIP;
		myAgent.addBehaviour(new LeaderElectionInitiator(myAgent));
	}
	
	/**
	 * Set the timeout used to wait for responses to an attempt to become leader
	 * @param t the timeout used to wait for responses to an attempt to become leader
	 */
	public void setProposalResponseTimeout(long t) {
		proposalResponseTimeout = t;
	}
	
	/**
	 * Set the maximum interval between two successive attempts to become leader. The actual
	 * interval will be a random amount of time between 0 and this value.
	 * @param t the maximum interval between two successive attempts to become leader
	 */
	public void setProposalRetryInterval(long t) {
		proposalRetryInterval = t;
	}

	/**
	 * This callback method is invoked whenever a new leader is elected. 
	 * The default implementation is empty. Applications can override it to react to this event properly.
	 */
	protected void leaderElected(AID leader) {
	}
	
	private void acquireLeadership() {
		status = LEADERSHIP_ACQUIRED;
		leader = myAgent.getAID();
		
		// Notify the leadership to all other agents
		sendLeadershipMessage(ACLMessage.INFORM, null);
		
		leaderElected(leader);
	}
	
	private void handleLeadershipNotification(ACLMessage inform) {
		try {
			Leader l = (Leader) myAgent.getContentManager().extractContent(inform);
			AID oldLeader = leader; 
			leader = l.getName();
			// Check the up-time
			if (l.getAge() < (System.currentTimeMillis() - upTime)) {
				// Should never happen
				System.out.println("WARNING: New leader "+leader.getLocalName()+" is younger than me");
			}
			status = LEADERSHIP_KNOWN;
			if (!leader.equals(oldLeader)) {
				leaderElected(leader);
			}
		}
		catch (Exception e) {
			// Should never happen
			e.printStackTrace();
		}
	}
	
	/**
	 * Inner class LeaderElectionResponder
	 */
	private class LeaderElectionResponder extends CyclicBehaviour {
		private MessageTemplate template = MessageTemplate.MatchTopic(leaderElectionTopic);

		private LeaderElectionResponder(Agent a) {
			super(a);
		}
		
		public void action() {
			ACLMessage msg = myAgent.receive(template);
			if (msg != null) {
				// Ignore messages sent by myself
				if (!msg.getSender().equals(myAgent.getAID())) {
					switch (msg.getPerformative()) {
					case ACLMessage.PROPOSE:
						//System.out.println("Agent "+myAgent.getLocalName()+" - PROPOSE message received. Status is "+status);
						handlePropose(msg);
						break;
					case ACLMessage.INFORM:
						//System.out.println("Agent "+myAgent.getLocalName()+" - INFORM message received. Status is "+status);
						handleLeadershipNotification(msg);
						break;
					}
				}
			}
			else {
				block();
			}
		}
		
		private void handlePropose(ACLMessage propose) {
			ACLMessage reply = propose.createReply();
			if (status != LEADERSHIP_ACQUIRED) {
				try {
					Leader l = (Leader) myAgent.getContentManager().extractContent(propose);
					long myAge = System.currentTimeMillis() - upTime;
					if (l.getAge() < myAge) {
						// The remote agent is younger than me --> Reject his proposal to become leader
						reply.setPerformative(ACLMessage.REJECT_PROPOSAL);
					}
					else if (l.getAge() == myAge) {
						// The remote agent is as old as me --> Accept his proposal to become leader unless I am already proposing myself
						reply.setPerformative(status == ACQUIRING_LEADERSHIP ? ACLMessage.REJECT_PROPOSAL : ACLMessage.ACCEPT_PROPOSAL);
					}
					else {
						// The remote agent is elder than me --> Accept his proposal to become leader
						reply.setPerformative(ACLMessage.ACCEPT_PROPOSAL);	
					}
				}
				catch (Exception e) {
					// Should never happen
					e.printStackTrace();
				}
			}
			else {
				// I am the leader! Just inform the remote agent
				reply.setPerformative(ACLMessage.INFORM);
				Leader l = new Leader(myAgent.getAID(), System.currentTimeMillis() - upTime);
				try {
					myAgent.getContentManager().fillContent(reply, l);
				}
				catch (Exception e) {
					// Should never happen
					e.printStackTrace();
				}
			}
			myAgent.send(reply);
		}
	} // END of inner class LeaderElectionResponder

	
	/**
	 * Inner class LeaderElectionInitiator
	 * This implements the leader election procedure as below:
	 *                START
	 *                  |
	 *                  V
	 *   ----------> TRY_ACQUIRE ------ (Otherwise) 
	 *  |               |              |  
	 *  |               |(WAITING)     | 
	 *  |(ACQUIRING     |              |
	 *  | LEADERSHIP)   V              |
	 *  |             WAIT             |
	 *  |               |              | 
	 *  |               |(Otherwise)   V
	 *   --------------- ------------>END     
	 */
	private class LeaderElectionInitiator extends FSMBehaviour {
		private LeaderElectionInitiator(Agent a) {
			super(a);
			
			registerFirstState(new SingleAttemptLeaderElectionInitiator(a), TRY_ACQUIRE_STATE);
			registerState(new WakerBehaviour(a, -1) {
				public void onStart() {
					// Get a random waiting time between 0 and proposalRetryInterval
					long interval = (long) (proposalRetryInterval * Math.random());
					if (interval == 0) {
						interval = proposalRetryInterval;
					}
					//System.out.println("--- Sleep time = "+interval);
					reset(interval);
				}
				
				public void onWake() {
					if (status == WAITING) {
						status = ACQUIRING_LEADERSHIP;
					}
				}
				
				public int onEnd() {
					return status;
				}
			}, WAIT_STATE);
			registerLastState(new OneShotBehaviour(a) {
				public void action() {
					// Just do nothing
				}
			}, DUMMY_FINAL_STATE);
			
			registerTransition(TRY_ACQUIRE_STATE, WAIT_STATE, WAITING);
			registerDefaultTransition(TRY_ACQUIRE_STATE, DUMMY_FINAL_STATE);
			registerTransition(WAIT_STATE, TRY_ACQUIRE_STATE, ACQUIRING_LEADERSHIP, new String[] {TRY_ACQUIRE_STATE, WAIT_STATE});
			registerDefaultTransition(WAIT_STATE, DUMMY_FINAL_STATE);
		}
	} // END of inner class LeaderElectionInitiator
	
	
	/**
	 * Inner class SingleAttemptLeaderElectionInitiator
	 */
	private class SingleAttemptLeaderElectionInitiator extends ParallelBehaviour {
		private boolean proposalAccepted = true;
		private MessageTemplate template;
		
		// Note that this behaviour runs until the timeout for receiving responses expires 
		// even if we receive a REJECT or an INFORM. In this way we sweep other responses 
		// (if any) that would stay in the queue forever.
		private SingleAttemptLeaderElectionInitiator(Agent a) {
			super(a, ParallelBehaviour.WHEN_ANY);
			
			addSubBehaviour(new CyclicBehaviour(a) {
				
				public void onStart() {
					String convId = buildConvId();
					sendLeadershipMessage(ACLMessage.PROPOSE, convId);
					// Avoid intercepting messages sent by myself. 
					// Such messages will be read and discarded by the LeaderElectionResponder
					template = MessageTemplate.MatchConversationId(convId);
				}
				
				public void action() {
					ACLMessage msg = myAgent.receive(template);
					if (msg != null) {
						// Ignore messages sent by myself
						if (!msg.getSender().equals(myAgent.getAID())) {
							switch (msg.getPerformative()) {
							case ACLMessage.ACCEPT_PROPOSAL:
								//System.out.println("Agent "+myAgent.getLocalName()+" - ACCEPT_PROPOSAL response received. Status is "+status);
								// Just do nothing
								break;
							case ACLMessage.REJECT_PROPOSAL:
								//System.out.println("Agent "+myAgent.getLocalName()+" - REJECT_PROPOSAL response received. Status is "+status);
								proposalAccepted = false;
								break;
							case ACLMessage.INFORM:
								//System.out.println("Agent "+myAgent.getLocalName()+" - INFORM response received. Status is "+status);
								// Someone other acquired the leadership
								handleLeadershipNotification(msg);
								break;
							}
						}
					}
					else {
						block();
					}
				}
			});
			
			addSubBehaviour(new WakerBehaviour(a, proposalResponseTimeout) {
				public void onWake() {
					if (status == ACQUIRING_LEADERSHIP && proposalAccepted) {
						// When the timeout expires, if no REJECT_PROPOSAL was received, and no one 
						// became the leader in the meanwhile, acquire the leadership.
						acquireLeadership();
					}
				}
			});
		}
		
		public int onEnd() {
			if (status == ACQUIRING_LEADERSHIP) {
				status = WAITING;
			}
			return status;
		}
		
		public void reset() {
			proposalAccepted = true;
			super.reset();
		}
	} // END of inner class SingleAttemptLeaderElectionInitiator
	
	
	/////////////////////////////////
	// Utility methods
	/////////////////////////////////
	private void sendLeadershipMessage(int performative, String conversationId) {
		ACLMessage msg = new ACLMessage(performative);
		msg.addReceiver(leaderElectionTopic);
		msg.setOntology(LeaderElectionOntology.getInstance().getName());
		msg.setLanguage(FIPANames.ContentLanguage.FIPA_SL);
		msg.setConversationId(conversationId);
		Leader l = new Leader(myAgent.getAID(), System.currentTimeMillis() - upTime);
		try {
			myAgent.getContentManager().fillContent(msg, l);
			myAgent.send(msg);
		}
		catch (Exception e) {
			// Should never happen
			e.printStackTrace();
		}
	}

	private int cnt = 0;
	private String buildConvId() {
		return "LE-"+myAgent.getLocalName()+"-"+(cnt++);
	}
}
