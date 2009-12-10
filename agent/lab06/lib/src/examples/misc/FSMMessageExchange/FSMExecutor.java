/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2003 TILAB S.p.A. 

GNU Lesser General Public License

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation, 
version 2.1 of the License. 

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the
Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA  02111-1307, USA.
*****************************************************************/

package examples.misc.FSMMessageExchange;

import jade.core.Agent;
import jade.core.behaviours.CyclicBehaviour;
import jade.lang.acl.ACLMessage;
import jade.lang.acl.MessageTemplate;
import jade.util.Logger;

import jade.misc.DynamicFSMBehaviour;

import java.io.StringBufferInputStream;
/**
 * This FSMExecutor class allows to show an example
 * of an agent requesting to another (this Executor)
 * to execute a certain FSM. 
 * <br> The Requestor describes the needed
 * FSM in XML and passes this description in the content of the
 * ACLMessage.
 * <br> The Executor parses the received description,
 * creates the needed FSM and executes it.
 * <br> In order to run this example, it is recommended to:
 * <ul>
 * <li> launch the JADE platform with this FSMExecutor agent:
 * <code>java -classpath lib\jade.jar;lib\jadeTools.jar;add-ons\misc\classes;lib\xerces.jar jade.Boot -gui -nomtp fsm:examples.misc.FSMMessageExchange.FSMExecutor </code>
 * <li> launch a DummyAgent 
 * <li> from the DummyAgent loads the sample message in the file 
 * <code>SampleMessage.txt </code> of this directory and set the proper
 * value for the receiver (i.e. <code>fsm</code> as a local agent)
 * <li> send this message to fsm.
 * </ul>
 * When this message arrives a new FSM is created and activated as a
 * behaviour of this agent. The sample FSM described in the sent message 
 * is very simple and it is composed of 3 states:
 * <ul>
 * <li> START state where the agent just waits until a message arrives (!! take
 * care of not sending a message whose ontology field was set to "FSMOntology"
 * otherwise it might be interpreted as the description of a new FSM!!)
 * <li> RX message where the agent waits until a message arrives. If an 
 * INFORM message arrives, then it remains in this RX state, otherwise it
 * goes to the END state
 * <li> END state where the agents prints on stdout that it has executed the state
 * </ul>
 * Now it should be obvious that the DummyAgent should be used to interact
 * with this FSMExecutor agent by sending proper messages that stimulate
 * the FSM.
 * <p> Note of the author: I implemented this example with the hope that 
 * someone else might dedicate some effort to improve it and make this
 * improvements available to the entire community.
 *
 * @author Fabio Bellifemine - TILAB
 * @version $Date: 2004-07-19 18:20:54 +0200 (lun, 19 lug 2004) $ $Revision: 438 $
 **/
public class FSMExecutor extends Agent {

    /**
     * This cyclic behaviour waits for a new request to create an FSM, 
     * parses the FSM, instantiate it, and add the Behaviour to the
     * scheduler of behaviour of this Agent.
     **/
    private static Logger logger = Logger.getMyLogger(FSMExecutor.class.getName()); 
     
    class FSMDescriptionReceiver extends CyclicBehaviour {
	private ACLMessage msg;
	private MessageTemplate msgTemplate = MessageTemplate.MatchOntology("FSMOntology");
	public void action() {
	    msg = myAgent.receive(msgTemplate);
	    if (msg == null) {
		block();
	    } else {
		DynamicFSMBehaviour fsm = new DynamicFSMBehaviour(myAgent, new StringBufferInputStream(msg.getContent()));
		if (fsm.getLoaded()) {
		    myAgent.addBehaviour(fsm);
		    if(logger.isLoggable(Logger.INFO))
		    	logger.log(Logger.INFO,"LOADED A NEW FSM: "+msg);
		} else{
		    if(logger.isLoggable(Logger.INFO))
		    	logger.log(Logger.INFO,"FAILED TO LOAD THIS FSM!");}
	    }
	}
    }
    protected void setup() {
	addBehaviour(new FSMDescriptionReceiver());
    }
}
