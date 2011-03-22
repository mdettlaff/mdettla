package mdettla.javadepend;

import java.util.ArrayList;
import java.util.Collection;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class JDClass {

	private final String packageName;
	private final String simpleClassName;
	private final String sourceCode;
	private Collection<JDClass> immediateDependenciesCache;

	public static JDClass fromJavaSourceCode(String sourceCode)
	throws JavaDependException {
		String packageName = parsePackageName(sourceCode);
		String simpleClassName = parseSimpleClassName(sourceCode);
		return new JDClass(packageName, simpleClassName, sourceCode);
	}

	private JDClass(String packageName, String simpleClassName) {
		this(packageName, simpleClassName, null);
	}

	private JDClass(String packageName, String simpleClassName, String sourceCode) {
		this.packageName = packageName;
		this.simpleClassName = simpleClassName;
		this.sourceCode = sourceCode;
	}

	public String getFullName() {
		return packageName + "." + simpleClassName;
	}

	public Collection<JDClass> getImmediateDependencies(
			Collection<JDClass> allClasses) {
		if (immediateDependenciesCache == null) {
			Collection<JDClass> dependencies = new ArrayList<JDClass>();
			dependencies.addAll(getImmediateDependenciesFromImports(allClasses));
			dependencies.addAll(getImmediateDependenciesFromTheSamePackage(allClasses));
			immediateDependenciesCache = dependencies;
		}
		return immediateDependenciesCache;
	}

	private Collection<JDClass> getImmediateDependenciesFromTheSamePackage(
			Collection<JDClass> allClasses) {
		Collection<JDClass> dependencies = new ArrayList<JDClass>();
		for (JDClass clazz : allClasses) {
			boolean isPackageMatching = packageName.equals(clazz.packageName);
			// use regex only if necessary (optimization)
			boolean hasReference = sourceCode.contains(clazz.simpleClassName)
				&& Pattern.compile("\\b" + clazz.simpleClassName +
						"\\b").matcher(sourceCode).find();
			boolean isSelf = equals(clazz);
			if (isPackageMatching && hasReference && !isSelf) {
				dependencies.add(clazz);
			}
		}
		return dependencies;
	}

	private Collection<JDClass> getImmediateDependenciesFromImports(
			Collection<JDClass> allClasses) {
		Collection<JDClass> dependencies = new ArrayList<JDClass>();
		Pattern importRegex = Pattern.compile("import ([\\w.]+)\\.(\\w+)");
		for (String line : sourceCode.split("\n")) {
			Matcher matcher = importRegex.matcher(line);
			if (matcher.find()) {
				JDClass jdClass = new JDClass(matcher.group(1), matcher.group(2));
				if (allClasses.contains(jdClass)) {
					dependencies.add(jdClass);
				}
			}
		}
		return dependencies;
	}

	private static String parsePackageName(String sourceCode)
		throws JavaDependException {
		Pattern packageNameRegex = Pattern.compile("package ([\\w.]+)");
		for (String line : sourceCode.split("\n")) {
			Matcher matcher = packageNameRegex.matcher(line);
			if (matcher.find()) {
				return matcher.group(1);
			}
		}
		throw new JavaDependException("Package name not found.");
	}

	private static String parseSimpleClassName(String sourceCode)
		throws JavaDependException {
		Pattern classNameRegex = Pattern.compile(
				"^(public |abstract |final )*(class|interface|enum) (\\w+)");
		for (String line : sourceCode.split("\n")) {
			Matcher matcher = classNameRegex.matcher(line);
			if (matcher.find()) {
				return matcher.group(3);
			}
		}
		throw new JavaDependException("Class, interface or enum name not found.");
	}

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof JDClass)) {
			return false;
		}
		JDClass other = (JDClass)obj;
		return getFullName().equals(other.getFullName());
	}

	@Override
	public int hashCode() {
		return getFullName().hashCode();
	}

	@Override
	public String toString() {
		return getClass().getSimpleName() + "[" + getFullName() + "]";
	}
}
