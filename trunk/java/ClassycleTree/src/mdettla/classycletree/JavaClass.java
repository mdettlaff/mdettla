package mdettla.classycletree;

import java.util.HashSet;
import java.util.Set;

public class JavaClass {

	private final String name;
	private Set<JavaClass> directDependencies;

	public JavaClass(String name) {
		this.name = name;
		this.directDependencies = new HashSet<JavaClass>();
	}

	public String getName() {
		return name;
	}

	public void addDirectDependency(JavaClass directDependency) {
		directDependencies.add(directDependency);
	}

	public Set<JavaClass> getDirectDependencies() {
		return new HashSet<JavaClass>(directDependencies);
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "[" +
		"name=" + name + ", " +
		"directDependenciesCount=" + directDependencies.size() +
		"]";
	}
}
