/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2002 TILAB

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

package jade.misc;

import jade.core.Agent;
import jade.core.AID;
import jade.domain.FIPANames;
import jade.domain.DFGUIManagement.*;
import jade.domain.JADEAgentManagement.*;	   
import jade.domain.FIPAAgentManagement.DFAgentDescription;	   
import jade.content.ContentManager;
import jade.content.lang.sl.SLCodec;
import jade.content.onto.basic.Action;
import jade.lang.acl.ACLMessage;
import jade.proto.AchieveREInitiator;
import jade.util.Logger;

import java.io.FileInputStream;
import java.util.Properties;
import java.util.Enumeration;
import java.util.Vector;

/**
 * This agent provides a simple graphical interface to federate DF agents.
 * Moreover it uses a property file to create some federations between DF agents at startup.
 * This file is passed to the DFFederatorAgent as command line argument and has the form <br>
 * <code>
 SubDF1 = df@jade.cselt.it:1099/JADE , IOR:00002233 <br>
 RootDF = df@teschet.it:1099/JADE, http://teschet.it:8088/acc <br>
 SubDF2 = agentName, agentAddress <br>
 RootDF2 = agentName, agentAddress <br
   </code>
 * <br> The strings <code>SubDF</code> and <code>RootDF</code> MUST be used
 * as a prefix to identify couples of subDF (i.e. the DF that must be federated
 * with) and rootDF. <br>
 * The value for each key is in the form agentName, agentAddress separated
 * by a comma. <br>
 * Fhe file DFFederatorAgent.properties in the resources directory is
 * an example of a property file for this agent. <br>
 * A suggested command line to better understand this agent is the following:
 * <br><code>
   java -cp <jade classes + misc add-on classes> jade.Boot -gui subDF:jade.domain.df federator:jade.misc.DFFederatorAgent(resources\DFFederatorAgent.properties) </code>
 * 
 * @author Fabio Bellifemine - TILAB
 * @version $Date: 2008-10-14 14:02:18 +0200 (mar, 14 ott 2008) $ $Revision: 1560 $
 * @see <a href="../../../../resources/DFFederatorAgent.properties">sample properties file </a>
 **/
public class DFFederatorAgent extends Agent {
	private DFFederatorAgentGUI myGui;	
	
	private static Logger logger = Logger.getMyLogger(DFFederatorAgent.class.getName());
	 
    /* This is the prefix string that identifies the key of a property
     * that represent a sub DF. The postfix after this key is appended
     * also to RootDFKeyPrefix to get the key of the property that
     * represents the rootDF for this subDF.
     */
    private static String SubDFKeyPrefix = "SubDF";
    /* @see SubDFKeyPrefix **/
    private static String RootDFKeyPrefix = "RootDF";

    protected void setup() {
	// Register the DFAppletOntology with the content manager
	getContentManager().registerOntology(DFAppletOntology.getInstance());
	// Register the JADEManagementOntology with the content manager
	getContentManager().registerOntology(JADEManagementOntology.getInstance());
	// Register the SL0 content language
	getContentManager().registerLanguage(new SLCodec(0), FIPANames.ContentLanguage.FIPA_SL0);	

	myGui = new DFFederatorAgentGUI(this);
	myGui.showCorrect();
	
	// read the name of the file (if any) that contains the specification
	// of the initial federations
	Object[] args = getArguments();
	if (args != null && args.length > 0) {
		String fileName = (String)(args[0]);
		// load the list of properties from the file
		Properties p = new Properties();
		try {
		    p.load(new FileInputStream(fileName));
		} catch (Exception e) {
		    System.err.println("Some problems in reading the list of properties for "+getLocalName());
		    e.printStackTrace();
		}
		// dump this list of properties on standard output
		//System.out.println("List of properties for "+getLocalName());
		//p.list(System.out);    
	
		for (Enumeration e=p.propertyNames(); e.hasMoreElements(); ) {
		    String key = (String)e.nextElement();
		    if (key.startsWith(SubDFKeyPrefix)) {
			String value = p.getProperty(key);
			AID childDF = createAID(value);
			myGui.addDF(childDF);
			String rootKey = createRootDFKey(key);
			value = p.getProperty(rootKey);
			AID parentDF = createAID(value);
			myGui.addDF(parentDF);
			//System.out.println("FEDERATE " + childDF.toString() + "\n WITH " + parentDF.toString());
			//federate(childDF, parentDF);
			requestFederation(childDF, parentDF);
		    }
		}
	}
	
    }
    
    protected void takeDown() {
    	if (myGui != null) {
    		myGui.dispose();
    	}
    }

    //////////////////////////////////
    // Methods called by the GUI
    //////////////////////////////////
    
    void requestShowGui(final AID df) {
    	Action action = new Action(df, new ShowGui());
			ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
			request.addReceiver(df);
			request.setLanguage(FIPANames.ContentLanguage.FIPA_SL0);	
			request.setOntology(JADEManagementOntology.NAME);
			try {
	    	getContentManager().fillContent(request, action);
    		addBehaviour(new AchieveREInitiator(this, request) {
    			protected void handleFailure(ACLMessage failure) {
    				String msg = df.getName()+" GUI activation failed ["+failure.getContent()+"]"; 
    				myGui.notifyFailure(msg);
    			}
    		} );
			}
			catch (Exception e) {
				e.printStackTrace();
				String msg = df.getName()+" GUI activation failed ["+e.getMessage()+"]"; 
				myGui.notifyFailure(msg);
			}
		}	
		
    void requestFederation(final AID childDF, final AID parentDF) {
			logger.log(Logger.INFO,"Federating "+childDF.getName()+" with "+parentDF.getName());
			Federate f = new Federate();
			f.setDf(parentDF);
			Action action = new Action(childDF, f);
			ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
			request.addReceiver(childDF);
			request.setLanguage(FIPANames.ContentLanguage.FIPA_SL0);	
			request.setOntology(DFAppletOntology.NAME);
			try {
	    	getContentManager().fillContent(request, action);
    		addBehaviour(new AchieveREInitiator(this, request) {
    			protected void handleInform(ACLMessage inform) {
    				myGui.notifyFederationOK(childDF, parentDF);
    			}
    			protected void handleFailure(ACLMessage failure) {
						String msg = "Federation between "+childDF.getName()+" and "+parentDF.getName()+" failed ["+failure.getContent()+"]";
    				myGui.notifyFailure(msg);
    			}
    		} );
			}
			catch (Exception e) {
				e.printStackTrace();
				String msg = "Federation between "+childDF.getName()+" and "+parentDF.getName()+" failed ["+e.getMessage()+"]";
				myGui.notifyFailure(msg);
			}
		}	
				
    void requestFederationRemoval(final AID childDF, final AID parentDF) {
			logger.log(Logger.INFO,"Removing federation between "+childDF.getName()+" and "+parentDF.getName());
			DeregisterFrom d = new DeregisterFrom();
			d.setDf(parentDF);
			DFAgentDescription dfd = new DFAgentDescription();
			dfd.setName(childDF);
			d.setDescription(dfd);
			Action action = new Action(childDF, d);
			ACLMessage request = new ACLMessage(ACLMessage.REQUEST);
			request.addReceiver(childDF);
			request.setLanguage(FIPANames.ContentLanguage.FIPA_SL0);	
			request.setOntology(DFAppletOntology.NAME);
			try {
	    	getContentManager().fillContent(request, action);
    		addBehaviour(new AchieveREInitiator(this, request) {
    			protected void handleInform(ACLMessage inform) {
    				myGui.notifyFederationRemoved(childDF, parentDF);
    			}
    			protected void handleFailure(ACLMessage failure) {
						String msg = "Federation removal between "+childDF.getName()+" and "+parentDF.getName()+" failed ["+failure.getContent()+"]";
    				myGui.notifyFailure(msg);
    			}
    		} );
			}
			catch (Exception e) {
				e.printStackTrace();
				String msg = "Federation removal between "+childDF.getName()+" and "+parentDF.getName()+" failed ["+e.getMessage()+"]";
				myGui.notifyFailure(msg);
			}
    }

    ////////////////////////////////////
    // Private utility methods
    ////////////////////////////////////
    
    /**
     * Parses the passed string containing an agent name and an agent address
     * separated by a comma and creates an AID for that agent.
     * @param value a String in the form <name>,<value> For instance
     * df@sharon.cselt.it:1099/JADE , IOR:000002233
     * @return the agent AID
     **/
    private AID createAID(String value) {
	AID aid;
	// before the comma is the AID.name, after the comma is the AID.address
	int ind = value.indexOf(',');
	if (ind < 0) {
	    // if no comma was found, then assume is a local agent
	    aid = new AID(value, (value.indexOf('@')<0 ? AID.ISLOCALNAME : AID.ISGUID));
	} else {
	    String aidName = value.substring(0,ind).trim();
	    String aidAddress = value.substring(ind+1).trim();
	    aid = new AID(aidName, AID.ISGUID);
	    aid.addAddresses(aidAddress);
	}
	return aid;
    }

    /**
     * parses the key for a subDF and return the corresponding key for the rootDF
     **/
    private String createRootDFKey(String subDFKey) {
	return RootDFKeyPrefix + subDFKey.substring(SubDFKeyPrefix.length());
    }
	
}
