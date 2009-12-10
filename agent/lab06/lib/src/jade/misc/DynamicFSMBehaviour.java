/*****************************************************************
JADE - Java Agent DEvelopment Framework is a framework to develop 
multi-agent systems in compliance with the FIPA specifications.
Copyright (C) 2002 donated to TILAB

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
import jade.core.behaviours.Behaviour;
import jade.core.behaviours.FSMBehaviour;
import jade.core.behaviours.OneShotBehaviour;
import java.io.InputStream;
import jade.util.Logger;

import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Base class for all FSM behaviours in OntoLogging.
 * It is automatically configured upon creation using an XML
 * state transition descriptor.<br/>
 * The descriptor defines the individual states of the FSM, 
 * the behaviour that is invoked in each one and the transition table 
 * (i.e. the next state of the FSM, depending on the exit code of 
 * the current state). Be default, the class searches for the file 
 * <class_name>.desc in the CLASSPATH (loaded as resource). So, for example class
 * com.test.ComplexBehaviour will search for file com.test.ComplexBehaviour.desc.<br/>
 * The XML file structure is still under development. It currently 
 * has the following structure:<br/><code>
 * <fsm>
 * &nbsp;&nbsp;<state name=<i>"aState"</i> first="true" behaviour=<i>"com.foo.ABehaviour"</i>>
 * &nbsp;&nbsp;&nbsp;&nbsp;<transition code=<i>"1"</i> nextState=<i>"anotherState"</i>/>
 * &nbsp;&nbsp;</state>
 * &nbsp;&nbsp;<state name=<i>"anotherState"</i> behaviour=<i>"com.foo.ANewBehaviour"</i>>
 * &nbsp;&nbsp;&nbsp;&nbsp;<transition code=<i>"3"</i> nextState=<i>"thirdState"</i>/>
 * &nbsp;&nbsp;&nbsp;&nbsp;<transition code=<i>"4"</i> nextState=<i>"endState"</i>/>
 * &nbsp;&nbsp;</state>
 * &nbsp;&nbsp;<state name=<i>"thirdState"</i>>
 * &nbsp;&nbsp;&nbsp;&nbsp;<transition nextState=<i>"endState"</i>/>
 * &nbsp;&nbsp;</state>
 * &nbsp;&nbsp;<state name=<i>"endState"</i> last="true" behaviour=<i>"com.foo.FinalBehaviour"</i>>
 * &nbsp;&nbsp;</state>
 * </fsm></code>
 * Some rules for the syntax of the XML file:<br/>
 * - One and only one of the <i>state</i> tags should have the <i>first</i> property; the same applies
 * for the <i>last</i> property. If more than one such elements exist, only the last one is taken 
 * into account.<br/>
 * - If the attribute <i>first</i> and <i>last</i> exist in the same <i>state</i> element, then
 * only <i>first</i> is taken into account.<br/>
 * - If the <i>behaviour</i> property is missing or the class is not existent, a default behaviour
 * that does nothing is used.<br/>
 * - If the <i>code</i> property is missing, the transition is considered a default one. The code
 * property's value should always be a valid integer. If it is not an integer, then the transition
 * becomes default.<br/>
 * - If the <i>nextState</i> attribute is missing, the transition is ignored altogether.<br/>
 * - Children of <i>state</i> other than <i>transition</i> are ignored.<br/>
 * - A <i>state</i> can have more than one transitions. Care should be given for the transitions 
 * of the same state not to have common values for their <i>code</i> property.<br/>
 * - If the <i>fsm</i> node has a child other than <i>state</i>, that child is ignored.<br/>
 * - If a <i>state</i> node has a child other than <i>transition</i> it is also ignored.<br/>
 * - If the <i>name</i> property is missing, then the states are assigned by default a name
 * StateXX, where XX is an outo-increment number<br/><br/> 
 * 
 * It should be noted that the syntax rules are not enforced; it is the developer's responsibility
 * to create a valid descriptor. This also implies that consistency between state names and 
 * avoidance of dead-ends loops should also be ensured.
 * @author Stelios Gerogiannakis, (c) Archetypon S.A. 2002, Project Onto-Logging 
 */
public class DynamicFSMBehaviour extends FSMBehaviour {

	private static Logger logger = Logger.getMyLogger(DynamicFSMBehaviour.class.getName());
	
	/**flag to show if states have been loaded successfully*/
	private boolean loaded = false;

	/**
	 * Default constructor.
	 * Does not initialize the internal state from the file. 
	 */
	public DynamicFSMBehaviour() {
		super();
	}

	/**
	 * Initializes the internal state table from the descriptor.
	 */
	public DynamicFSMBehaviour(Agent arg0) {
		super(arg0);
		
		loaded = loadStates();
	}

    /**
     * This constructor constructs an FSMBehaviour that implements
     * the FSM represented in the XML document read by the InputStream.
     * Notice that the passed InputStream is not closed by this class
     * and should be closed by the caller.
     * @param myAgent the Agent that is going to execute this behaviour
     * @param xmlDocument the InputStream containing the XML Document
     **/
    public DynamicFSMBehaviour(Agent myAgent, InputStream xmlDocument) {
	super(myAgent);

	loaded = loadStates(createDomDocument(xmlDocument));
    }


	/**
	 * Overrides the parent method.
	 * If the states have not already been loaded from the descriptor,
	 * load them now.
	 */
	public void setAgent(Agent agent) {
		super.setAgent(agent);

		if(! loaded)		
			loaded = loadStates();		
	}


	/**
	 * Gets if thae last attempt to load the transition matrix from the 
	 * descriptor has completed successfully.
	 * @return Returns a boolean
	 */
	public boolean getLoaded() {
		return loaded;
	}

	/**
	 * Loads that states from the same-named file.
	 * @return boolean true if succedded; false if file does not 
	 * exist or is corrupted
	 */ 
	protected boolean loadStates() {
		Document doc = null;
		
		if((doc = createDomDocument(this)) == null)
			return false;
		return loadStates(doc);
		
	} 

    /**
     * Loads the states of the FSM from the given DOM Document
     * @return boolean true if succedded; false otherwise 
     **/
    private boolean loadStates(Document doc) {
	        Element root = null;

		root = doc.getDocumentElement();
		if(! root.getTagName().equals("fsm"))
			return false;
		
		if(! scanStates(root))
			return false;
		
		return true;
    }

	/**
	 * Utility method.
	 * Takes an Object as argument, creates a file name <class_name>.desc
	 * and parses that file into a DOM Document. If the object is null, 
	 * it returns null;
	 * @param obj
	 * @return Document if parsing fails, it returns null
	 */
	private Document createDomDocument(Object obj) {
		String name = null;
		Document doc = null;
		
		if(obj != null)
			name = obj.getClass().getName();
		else
			return null;
		
		try {
			InputStream is = this.getClass().getClassLoader().getResourceAsStream(name +".desc");
			
			doc = createDomDocument(is); 
			
		}catch(Exception e) {
			return null;
		}
		
		return doc;
	}

	/**
	 * Utility method.
	 * Parses the passed InputStream and returns a DOM Document. 
	 * @return Document; if parsing fails, it returns null
	 */
	private Document createDomDocument(InputStream is) {
		try {
		    return DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(is);
		}catch(Exception e) {
		    return null;
		}
	}
	
	/**
	 * Utility method.
	 * Takes a DOM node (the root) as argument and scans it according to the 
	 * rules defined in the class documentation.
	 * @param root
	 * @return boolean flase if the elements do not follow the expected structure
	 */
	private boolean scanStates(Element root) {
		NodeList children = null; 
		
		if(! root.hasChildNodes())
			return false;
			
		children = root.getChildNodes();
		
		for(int i = 0; i < children.getLength(); i++) {
			createState(children.item(i), i);
		}
		
		
		return true;
	}
	
	/**
	 * Utility method.
	 * Takes a DOM Element and attempts to create a state with its behaviour.
	 * If the element is of type other than <i>state</i>, it is ignored.
	 * If the element is valid, but w/o a name, this is created automatically 
	 * using the provided integer.
	 */
	private void createState(Node stateNode, int counter) {
		String name = null;
		Attr attribute = null;
		boolean first = false, last = false, loaded = false;
		Behaviour behaviour = null;
		Element state = null;
		
		// check if this is an Element; if not (e.g. comment) return
		try {			
			state = (Element)stateNode;
		}catch(Exception e) {
			return;
		}
		
		if(! state.getTagName().equals("state"))
			return;
		
		// state name	
		if((attribute = state.getAttributeNode("name")) == null)
			name = "State" + counter;
		else
			name = attribute.getValue();
			
		// is it first?
		if((attribute = state.getAttributeNode("first")) != null)
			first = true;
		// is it last?
		else if((attribute = state.getAttributeNode("last")) != null)
			last = true;
			
		// load the behaviour
		behaviour = loadBehaviour(state, name);
		
		// register the state
		// is it the 1st one?
		if(first)
			this.registerFirstState(behaviour, name);
		// is it the last one?
		else if(last)
			this.registerLastState(behaviour, name);
		// this is a normal one
		else
			this.registerState(behaviour, name);
		
		// load and register the transitions
		scanTransitions(state, name);
		
	}
	
	/**
	 * Utility method.
	 * Takes a <i>state</i> Element as argument and goes through its <i>transition</i>
	 * children, if existent. After checking them one-by-one, it creates the transition 
	 * matrix. This method does not perform any semantic checks for the validity of the 
	 * matrix.
	 * @param state
	 * @param name of the state (it may be a default one)
	 */
	private void scanTransitions(Element state, String name) {
		NodeList children = null; 
		
		if(! state.hasChildNodes())
			return;
			
		children = state.getChildNodes();
		
		for(int i = 0; i < children.getLength(); i++) {
			createTransition(children.item(i), name);
		}
		
	}
	
	
	/**
	 * Utility method.
	 * Takes an Element and a state name as input. If the Element is not of type
	 * <i>transition</i> it is ignored. If it is, but does not have a <i>nextState</i>
	 * attribute, it is also ignored.
	 * @param transitionNode
	 * @param currState
	 */
	private void createTransition(Node transitionNode, String currState) {
		String trName = null, nextState = null;
		Attr attribute = null;
		Element transition = null;
		int code = Integer.MIN_VALUE;
		
		// check if an Element; if not (e.g. comment) return
		try {
			transition = (Element)transitionNode;
		}catch(Exception e) {
			return;
		}
		
		trName = transition.getTagName();
		
		if(! trName.equals("transition"))
			return;
		
		if((attribute = transition.getAttributeNode("nextState")) == null)
			return;
		
		nextState = attribute.getValue();
		
		// get transition code		
		if((attribute = transition.getAttributeNode("code")) != null)
			try {
				code = Integer.parseInt(attribute.getValue());
			}catch(Exception e) {}
		
		// this is a normal transition
		if(code != Integer.MIN_VALUE)
			this.registerTransition(currState, nextState, code);
		// this is a default transition
		else
			this.registerDefaultTransition(currState, nextState);		
	}
	
	/**
	 * Utility method.
	 * Takes a DOM entity and searches for the <i>behaviour</i> atribute.
	 * If it exists, it attempts to load the class that it describes in its
	 * value. If the attribute or the class do not exist, a default behaviour
	 * is created, printing out the state's name.
	 * @param state
	 * @param name of the state
	 * @return Behaviour that was created
	 */
	private Behaviour loadBehaviour(Element state, String name) {
		Attr attribute = null;
		boolean found = false;
		Behaviour beh = null;
		
		// search for the attribute
		if((attribute = state.getAttributeNode("behaviour")) != null) {
			// try to lad the class
			// if all goes well, flag will be set to true
			try {
				
				beh = (Behaviour)Class.forName(attribute.getValue()).newInstance();
				
				found = true;		
			}catch(Exception e) {
				found = false;
			}
		}
		
		// not found, make a quick class
		if(! found) {
			beh = new AuxiliaryBehaviour();
			
			((AuxiliaryBehaviour)beh).setStateName(name); 
		}		
		
		return beh;
	}
	
	
	/**
	 * Auxiliary behaviour, used as a fallback in cases of failure in dynamic loading.
	 */
	public class AuxiliaryBehaviour extends OneShotBehaviour {
		private String stateName = null;
		
		public void setStateName(String name) {
			this.stateName = name;
		}
		
		public void action() {
			logger.log(Logger.INFO,"Agent: " + myAgent + " is in state: " + this.stateName);
		}
	}

}

