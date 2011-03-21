package mdettla.javadepend;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class JDClassTest {

	@Test
	public void fromJavaSourceCode() throws JavaDependException {
		final String sourceCode =
			"package foo.bar\n" +
			"public class A {\n" +
			"}";
		JDClass A = JDClass.fromJavaSourceCode(sourceCode);
		assertEquals("foo.bar.A", A.getFullName());
	}
}
