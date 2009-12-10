package jade.misc;

import jade.content.onto.*;
import jade.content.schema.*;

/**
 */
public class LeaderElectionOntology extends Ontology implements LeaderElectionVocabulary {
	// The singleton instance of this ontology
	private final static Ontology theInstance = new LeaderElectionOntology();
	
	/**
	 * Retrieve the singleton instance of the LeaderElectionOntology
	 * @return the singleton instance of the LeaderElectionOntology
	 */
	public final static Ontology getInstance() {
		return theInstance;
	}
	
	/**
	 * Constructor
	 */
	private LeaderElectionOntology() {
		super(ONTOLOGY_NAME, BasicOntology.getInstance(), new CFReflectiveIntrospector());
		
		try {
			add(new PredicateSchema(LEADER), Leader.class);
			
			// ATTRIBUTE
			PredicateSchema ps = (PredicateSchema) getSchema(LEADER);
			ps.add(LEADER_NAME, (ConceptSchema) getSchema(BasicOntology.AID), ObjectSchema.MANDATORY);
			ps.add(LEADER_AGE, (PrimitiveSchema) getSchema(BasicOntology.INTEGER), ObjectSchema.MANDATORY);
		}
		catch (OntologyException oe) {
			// Should never happen
			oe.printStackTrace();
		}
		catch (Exception e){
			// Should never happen
			e.printStackTrace();
		}
	}
}
