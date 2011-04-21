package mdettla.classycletree;

import static org.junit.Assert.assertEquals;

import java.io.Reader;
import java.io.StringReader;

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
		Reader reader = new StringReader(CLASSYCLE_XML);
		ClassycleTree classycleTree = new ClassycleTree(reader, "foo.Bar");

		String actual = classycleTree.getDependencyTree();
		Tree<String> expectedTree = new Tree<String>("foo.Bar");
		assertEquals(expectedTree.toString(), actual);
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
		Reader reader = new StringReader(CLASSYCLE_XML);
		ClassycleTree classycleTree = new ClassycleTree(reader, "foo.Bar");

		String actual = classycleTree.getDependencyTree();
		Tree<String> expectedTree = new Tree<String>("foo.Bar");
		expectedTree.addLeaf("baz.Qux");
		assertEquals(expectedTree.toString(), actual);
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
		Reader reader = new StringReader(CLASSYCLE_XML);
		ClassycleTree classycleTree = new ClassycleTree(reader, "foo.Bar");

		String actual = classycleTree.getDependencyTree();
		Tree<String> expectedTree = new Tree<String>("foo.Bar");
		expectedTree.addLeaf("baz.Qux");
		assertEquals(expectedTree.toString(), actual);
	}
}
