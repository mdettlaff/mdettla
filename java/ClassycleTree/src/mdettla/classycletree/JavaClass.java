package mdettla.classycletree;

import java.util.HashSet;
import java.util.Set;

public class JavaClass {

	private final String name;
	private Set<JavaClass> dependencies;

	public JavaClass(String name) {
		this.name = name;
		this.dependencies = new HashSet<JavaClass>();
	}

	public String getName() {
		return name;
	}

	public void addDependency(JavaClass dependency) {
		dependencies.add(dependency);
	}

	public Set<JavaClass> getImmediateDependencies() {
		return new HashSet<JavaClass>(dependencies);
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "[" +
		"name=" + name + ", " +
		"dependenciesCount=" + dependencies.size() +
		"]";
	}
}
