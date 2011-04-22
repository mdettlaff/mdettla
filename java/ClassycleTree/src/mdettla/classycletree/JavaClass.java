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

	public int getDependenciesCount() {
		Set<JavaClass> counted = new HashSet<JavaClass>();
		counted.add(this);
		return getDependenciesCount(counted);
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof JavaClass)) {
			return false;
		}
		JavaClass other = (JavaClass)obj;
		return getName().equals(other.getName());
	}

	@Override
	public int hashCode() {
		return getName().hashCode();
	}

	@Override
	public String toString() {
		return name + " (" + getDependenciesCount() + ")";
	}

	private int getDependenciesCount(Set<JavaClass> counted) {
		int dependenciesCount = 0;
		for (JavaClass dependency : getDirectDependencies()) {
			if (!counted.contains(dependency)) {
				counted.add(dependency);
				dependenciesCount += 1 + dependency.getDependenciesCount(counted);
			}
		}
		return dependenciesCount;
	}
}
