package mdettla.classycletree;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringReader;

import javax.xml.stream.XMLStreamException;

import org.junit.Test;

public class ClassycleTreeTest {

	@Test
	public void testGetNoDependencies() throws Exception {
		String CLASSYCLE_XML =
			"<?xml version='1.0' encoding='UTF-8'?>\n" +
			"<classycle>\n" +
			"\t<classes>\n" +
			"\t\t<class name=\"foo.Bar\" />\n" +
			"\t</classes>\n" +
			"</classycle>";
		String actual = getTreeFromXmlReport(CLASSYCLE_XML);

		Tree<String> barTree = new Tree<String>("foo.Bar");
		String expected = barTree.toString();
		assertEquals(expected, actual);
	}

	@Test
	public void testGetSingleDependency() throws Exception {
		String CLASSYCLE_XML =
			"<?xml version='1.0' encoding='UTF-8'?>\n" +
			"<classycle>\n" +
			"\t<classes>\n" +
			"\t\t<class name=\"foo.Bar\">\n" +
			"\t\t\t<classRef name=\"ignore.This\" type=\"usedBy\" />\n" +
			"\t\t\t<classRef name=\"baz.Qux\" type=\"usesInternal\" />\n" +
			"\t\t</class>\n" +
			"\t\t<class name=\"baz.Qux\" />\n" +
			"\t</classes>\n" +
			"</classycle>";
		String actual = getTreeFromXmlReport(CLASSYCLE_XML);

		Tree<String> barTree = new Tree<String>("foo.Bar");
		barTree.addLeaf("baz.Qux");
		Tree<String> quxTree = new Tree<String>("baz.Qux");
		String expected = barTree + "\n\n" + quxTree;
		assertEquals(expected, actual);
	}

	@Test
	public void testGetCyclicDependency() throws Exception {
		String CLASSYCLE_XML =
			"<?xml version='1.0' encoding='UTF-8'?>\n" +
			"<classycle>\n" +
			"\t<classes>\n" +
			"\t\t<class name=\"foo.Bar\">\n" +
			"\t\t\t<classRef name=\"baz.Qux\" type=\"usesInternal\" />\n" +
			"\t\t</class>\n" +
			"\t\t<class name=\"baz.Qux\">\n" +
			"\t\t\t<classRef name=\"foo.Bar\" type=\"usesInternal\" />\n" +
			"\t\t</class>\n" +
			"\t</classes>\n" +
			"</classycle>";
		String actual = getTreeFromXmlReport(CLASSYCLE_XML);

		Tree<String> barTree = new Tree<String>("foo.Bar");
		barTree.addLeaf("baz.Qux");
		Tree<String> quxTree = new Tree<String>("baz.Qux");
		quxTree.addLeaf("foo.Bar");
		String expected = barTree + "\n\n" + quxTree;
		assertEquals(expected, actual);
	}

	private String getTreeFromXmlReport(String classycleXml)
			throws XMLStreamException, IOException {
		Reader reader = new StringReader(classycleXml);
		ClassycleTree classycleTree = new ClassycleTree(reader);

		ByteArrayOutputStream treeOut = new ByteArrayOutputStream();
		classycleTree.printDependencyTree(new PrintStream(treeOut));
		String actual = treeOut.toString();
		return actual;
	}
}
