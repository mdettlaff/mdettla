package mdettla.classycletree;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import classycle.Analyser;
import classycle.util.TrueStringPattern;

/**
 * Wyświetla drzewo zależności klas na podstawie raportu XML
 * wygenerowanego przez program
 * <a href="http://classycle.sourceforge.net/">Classycle</a>.
 */
public class ClassycleTree {

	private final Map<String, JavaClass> classPool;

	public ClassycleTree(Reader classycleXmlReport)
	throws XMLStreamException, IOException {
		try {
			classPool = getClassPool(classycleXmlReport);
		} finally {
			classycleXmlReport.close();
		}
	}

	public static void main(String[] args)
	throws XMLStreamException, IOException {
		if (args.length < 1) {
			System.err.println(
					"Usage: java " + ClassycleTree.class.getName() +
					" CLASS_FILES...");
			System.exit(2);
		}
		String[] classFiles = args;

		String xmlReport = getClassycleXmlReport(classFiles);
		Reader xmlReportReader = new StringReader(xmlReport);
		ClassycleTree classycleTree = new ClassycleTree(xmlReportReader);
		System.out.println();
		classycleTree.printDependencyTree(System.out);
	}

	private static String getClassycleXmlReport(String[] classFiles)
	throws IOException {
		final boolean mergeInnerClasses = true;
		Analyser analyser = new Analyser(
				classFiles, new TrueStringPattern(), null, mergeInnerClasses);
		final boolean packagesOnly = false;
		analyser.readAndAnalyse(packagesOnly);
		ByteArrayOutputStream xmlOut = new ByteArrayOutputStream();
		String title = classFiles[0];
		analyser.printXML(title, packagesOnly, new PrintWriter(xmlOut));
		String xmlReport = xmlOut.toString();
		return xmlReport;
	}

	public void printDependencyTree(PrintStream printer) throws XMLStreamException {
		List<JavaClass> classes = new ArrayList<JavaClass>(classPool.values());
		for (int i = 0; i < classes.size(); i++) {
			JavaClass javaClass = classes.get(i);
			Tree<JavaClass> dependencyTree = new Tree<JavaClass>(javaClass);
			addDependenciesToTree(dependencyTree, javaClass);
			printDependencyTree(dependencyTree, printer, i);
		}
	}

	private void printDependencyTree(
			Tree<JavaClass> dependencyTree, PrintStream printer, int i) {
		if (i > 0) {
			printer.print("\n\n");
		}
		printer.print(dependencyTree.toString());
	}

	private void addDependenciesToTree(
			Tree<JavaClass> dependencyTree, JavaClass javaClass) {
		for (JavaClass dependency : javaClass.getDirectDependencies()) {
			if (!dependencyTree.containsNode(dependency)) {
				dependencyTree.addLeaf(javaClass, dependency);
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
		boolean isInsideClasses = false;
		while (xmlReader.hasNext()) {
			int eventType = xmlReader.next();
			if (eventType == XMLStreamReader.START_ELEMENT) {
				String elementName = xmlReader.getLocalName();
				if ("classes".equals(elementName)) {
					isInsideClasses = true;
				} else if (isInsideClasses && "class".equals(elementName)) {
					xmlClassName = xmlReader.getAttributeValue(null, "name");
					ensureClassInPool(classPool, xmlClassName);
				} else if (isInsideClasses && "classRef".equals(elementName)
						&& "usesInternal".equals(xmlReader.getAttributeValue(
								null, "type"))) {
					String xmlDirectDependencyName =
						xmlReader.getAttributeValue(null, "name");
					handleClassRef(classPool, xmlClassName, xmlDirectDependencyName);
				}
			} else if (eventType == XMLStreamReader.END_ELEMENT
					&& "classes".equals(xmlReader.getLocalName())) {
				isInsideClasses = false;
			}
		}
		return Collections.unmodifiableMap(classPool);
	}

	private void handleClassRef(Map<String, JavaClass> classPool,
			String xmlClassName, String xmlDirectDependencyName) {
		if (xmlClassName == null) {
			throw new IllegalStateException("Wrong location of classRef element.");
		}
		ensureClassInPool(classPool, xmlDirectDependencyName);
		classPool.get(xmlClassName).addDirectDependency(
				classPool.get(xmlDirectDependencyName));
	}

	private void ensureClassInPool(
			Map<String, JavaClass> classPool, String className) {
		if (!classPool.containsKey(className)) {
			classPool.put(className, new JavaClass(className));
		}
	}
}
