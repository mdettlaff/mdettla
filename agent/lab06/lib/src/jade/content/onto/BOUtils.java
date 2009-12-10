package jade.content.onto;

import jade.content.onto.Ontology;
import jade.content.onto.OntologyException;
import jade.content.schema.ObjectSchema;
import jade.util.leap.Iterator;
import jade.util.leap.List;
import jade.util.leap.Set;

public class BOUtils {

	private static boolean compareSequences(Iterator i1, Iterator i2) {
		boolean result = true;

		Object o1, o2;
		while (i1.hasNext()) {
			o1 = i1.next();
			o2 = i2.next();
			result = (o1 == null ? o2 == null : o1.equals(o2));
			if (!result) {
				break;
			}
		}
		return result;
	}

	public static boolean leapListsAreEqual(List l1, List l2) {

		if (l1 == l2) {
			return true;
		}
		if (l1 == null) {
			return l2 == null;
		}
		if (l2 == null) {
			return false;
		}
		if (l1.size() != l2.size()) {
			return false;
		}

		return compareSequences(l1.iterator(), l2.iterator());
	}

	public static boolean leapSetsAreEqual(Set s1, Set s2) {

		if (s1 == s2) {
			return true;
		}
		if (s1 == null) {
			return s2 == null;
		}
		if (s2 == null) {
			return false;
		}
		if (s1.size() != s2.size()) {
			return false;
		}

		return compareSequences(s1.iterator(), s2.iterator());
	}

	private static void dumpSchemas(Ontology ontology, java.util.Iterator iter, String prefix) throws OntologyException {
		String name;
		ObjectSchema os;
		while (iter.hasNext()) {
			name = (String) iter.next();
			os = ontology.getSchema(name);
			System.out.print("  " + prefix + " " + name);
			ObjectSchema[] superSchemas = os.getSuperSchemas();
			if (superSchemas != null && superSchemas.length > 0) {
				System.out.print(" [ superschemas: ");
				for (int j = 0; j < superSchemas.length; j++) {
					System.out.print(superSchemas[j]+" ");
				}
				System.out.print("]");
			}
			System.out.println(" ::= {");
			String[] names = os.getNames();
			for (int i = 0; i < names.length; i++) {
				System.out.print("    " + names[i] + ": ");
				ObjectSchema schema = os.getSchema(names[i]);
				if (schema == null) {
					System.out.println("ERROR: no schema!");
				} else {
					System.out.println(schema.getTypeName());
				}
			}
			System.out.println("  }");
		}
	}

	public static void exploreOntology(Ontology ontology) throws OntologyException {
		System.out.println("\n\nOntology \"" + ontology.getName() + "\"\n");
		System.out.println("Concepts:");
		dumpSchemas(ontology, ontology.getConceptNames().iterator(), "concept");
		System.out.println("\nPredicates:");
		dumpSchemas(ontology, ontology.getPredicateNames().iterator(), "predicate");
		System.out.println("\nActions:");
		dumpSchemas(ontology, ontology.getActionNames().iterator(), "action");
	}
}
