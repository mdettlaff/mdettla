package mdettla.classycletree;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

/**
 * Wyświetla drzewo zależności podanej klasy na podstawie raportu XML
 * wygenerowanego przez program
 * <a href="http://classycle.sourceforge.net/">Classycle</a>.
 */
public class ClassycleTree {

	private final String className;
	private final Map<String, JavaClass> classPool;

	public ClassycleTree(Reader reader, String className)
	throws XMLStreamException, IOException {
		try {
			this.className = className;
			classPool = getClassPool(reader);
		} finally {
			reader.close();
		}
	}

	public static void main(String[] args)
	throws XMLStreamException, IOException {
		if (args.length < 2) {
			System.err.println("Usage: java " + ClassycleTree.class.getName() +
			" CLASSYCLE_XML_FILE CLASS_NAME");
			System.exit(2);
		}
		Reader reader = new InputStreamReader(new FileInputStream(args[0]));
		String className = args[1];
		ClassycleTree classycleTree = new ClassycleTree(reader, className);
		System.out.println(classycleTree.getDependencyTree());
	}

	public String getDependencyTree() throws XMLStreamException {
		JavaClass selectedClass = classPool.get(className);
		Tree<String> dependencyTree = new Tree<String>(selectedClass.getName());
		addDependenciesToTree(dependencyTree, selectedClass);
		return dependencyTree.toString();
	}

	private void addDependenciesToTree(
			Tree<String> dependencyTree, JavaClass javaClass) {
		for (JavaClass dependency : javaClass.getImmediateDependencies()) {
			String dependencyName = dependency.getName();
			if (!dependencyTree.containsNode(dependencyName)) {
				dependencyTree.addLeaf(javaClass.getName(), dependencyName);
				addDependenciesToTree(dependencyTree, dependency);
			}
		}
	}

	private Map<String, JavaClass> getClassPool(Reader reader)
	throws XMLStreamException {
		Map<String, JavaClass> classPool = new LinkedHashMap<String, JavaClass>();
		XMLInputFactory inputFactory = XMLInputFactory.newInstance();
		XMLStreamReader xmlReader = inputFactory.createXMLStreamReader(reader);
		String xmlClassName = null;
		while (xmlReader.hasNext()) {
			int eventType = xmlReader.next();
			if (eventType == XMLStreamReader.START_ELEMENT) {
				String elementName = xmlReader.getLocalName();
				if ("class".equals(elementName)) {
					xmlClassName = xmlReader.getAttributeValue(null, "name");
					ensureClassInPool(classPool, xmlClassName);
				} else if ("classRef".equals(elementName)
						&& "usesInternal".equals(
								xmlReader.getAttributeValue(null, "type"))) {
					String xmlImmediateDependencyName =
						xmlReader.getAttributeValue(null, "name");
					if (xmlClassName == null) {
						throw new IllegalStateException(
								"Wrong location of classRef element.");
					}
					ensureClassInPool(classPool, xmlImmediateDependencyName);
					classPool.get(xmlClassName).addDependency(
							classPool.get(xmlImmediateDependencyName));
				}
			}
		}
		return Collections.unmodifiableMap(classPool);
	}

	private void ensureClassInPool(
			Map<String, JavaClass> classPool, String className) {
		if (!classPool.containsKey(className)) {
			classPool.put(className, new JavaClass(className));
		}
	}
}
