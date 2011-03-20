package mdettla.javadepend;

import static org.junit.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;

public class JavaDependTest {

	@Test
	public void getImmediateDependencies() {
		JDClass A = JDClass.fromJavaSourceCode(
				"package foo;\n" +
				"import foo.bar.B\n" +
				"import foo.baz.C\n" +
				"import java.util.List\n" +
				"public class A {\n" +
				"}"
		);
		JDClass B = JDClass.fromJavaSourceCode("package foo.bar;\nclass B {}");
		JDClass C = JDClass.fromJavaSourceCode("package foo.baz;\nclass C {}");
		Collection<JDClass> allClasses = Arrays.asList(A, B, C);
		Collection<JDClass> expected = Arrays.asList(B, C);
		assertEquals(expected, A.getImmediateDependencies(allClasses));
	}

	@Test
	public void cyclicDependencies() {
		JDClass A = JDClass.fromJavaSourceCode(
				"package foo;\n" +
				"import foo.bar.B\n" +
				"public class A {\n" +
				"}"
		);
		JDClass B = JDClass.fromJavaSourceCode(
				"package foo;\n" +
				"import foo.bar.A\n" +
				"public class B {\n" +
				"}"
		);
		Collection<JDClass> allClasses = Arrays.asList(A, B);
		Collection<JDClass> expected = Arrays.asList(B);
		assertEquals(expected, JavaDepend.getAllDependencies(A, allClasses));
	}

	@Test
	public void getAllDependenciesFromImports() {
		JDClass A = JDClass.fromJavaSourceCode(
				"package foo;\n" +
				"import foo.bar.B\n" +
				"import foo.baz.C\n" +
				"import foo.baz.D\n" +
				"public class A {\n" +
				"}"
		);
		JDClass B = JDClass.fromJavaSourceCode(
				"package foo.bar;\n" +
				"public class B {\n" +
				"}"
		);
		JDClass C = JDClass.fromJavaSourceCode(
				"package foo.baz;\n" +
				"public class C {\n" +
				"}"
		);
		JDClass D = JDClass.fromJavaSourceCode(
				"package foo.baz;\n" +
				"import foo.qux.E\n" +
				"public class D {\n" +
				"}"
		);
		JDClass E = JDClass.fromJavaSourceCode(
				"package foo.qux;\n" +
				"public class E {\n" +
				"}"
		);
		JDClass F = JDClass.fromJavaSourceCode(
				"package foo.quux;\n" +
				"public class F {\n" +
				"}"
		);
		Collection<JDClass> allClasses = Arrays.asList(A, B, C, D, E, F);
		Collection<JDClass> actual = JavaDepend.getAllDependencies(A, allClasses);
		Collection<JDClass> expected = Arrays.asList(B, C, D, E);
		assertEquals(expected, actual);
	}

	@Test
	public void getAllDependenciesFromTheSamePackage() {
		JDClass A = JDClass.fromJavaSourceCode(
				"package foo.bar;\n" +
				"public class A {\n" +
				"    private B;\n" +
				"}"
		);
		JDClass B = JDClass.fromJavaSourceCode(
				"package foo.bar;\n" +
				"public class B {\n" +
				"}"
		);
		JDClass C = JDClass.fromJavaSourceCode(
				"package foo.bar;\n" +
				"public class C {\n" +
				"}"
		);
		JDClass D = JDClass.fromJavaSourceCode(
				"package foo.baz;\n" +
				"public class D {\n" +
				"}"
		);
		Collection<JDClass> allClasses = Arrays.asList(A, B, C, D);
		Collection<JDClass> actual = JavaDepend.getAllDependencies(A, allClasses);
		Collection<JDClass> expected = Arrays.asList(B);
		assertEquals(expected, actual);
	}
}
