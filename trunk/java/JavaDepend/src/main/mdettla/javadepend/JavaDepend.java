package mdettla.javadepend;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.io.FileUtils;

/**
 * Calculates number of dependencies for classes in a Java program.
 */
public class JavaDepend {

	private static final int USER_ERROR_CODE = 2;

	public static void main(String[] args) throws IOException {
		if (args.length == 0) {
			System.err.println("Usage: java " + JavaDepend.class.getName() +
					" JAVA_FILES...");
			System.exit(USER_ERROR_CODE);
		}
		Collection<File> allJavaFiles = getAllJavaFiles(args);
		if (allJavaFiles.size() > 0) {
			Collection<JDClass> allClasses = getAllClasses(allJavaFiles);
			int allDependenciesCount = 0;
			for (JDClass jdClass : allClasses) {
				Collection<JDClass> dependenciesForClass =
					getAllDependencies(jdClass, allClasses);
				allDependenciesCount += dependenciesForClass.size();
				System.out.println(jdClass.getFullName() + ": " +
						dependenciesForClass.size() + " dependencies");
				for (JDClass dependency : dependenciesForClass) {
					System.out.println("\tdepends on: " + dependency.getFullName());
				}
			}
			System.out.println();
			printSummary(allJavaFiles, allDependenciesCount);
		} else {
			System.err.println("No classes found.");
		}
	}

	private static Collection<JDClass> getAllClasses(
			Collection<File> allJavaFiles) throws IOException {
		Collection<JDClass> allClasses = new ArrayList<JDClass>();
		for (File javaFile : allJavaFiles) {
			String javaSourceCode = FileUtils.readFileToString(javaFile);
			try {
				allClasses.add(JDClass.fromJavaSourceCode(javaSourceCode));
			} catch (JavaDependException e) {
				System.err.println(javaFile.getPath() + ": " + e.getMessage());
			}
		}
		return allClasses;
	}

	private static void printSummary(Collection<File> allJavaFiles,
			int allDependenciesCount) {
		int allClassesCount = allJavaFiles.size();
		double averageDependenciesPerClass =
			((double)allDependenciesCount) / allClassesCount;
		double percentage = averageDependenciesPerClass / allClassesCount * 100;
		System.out.println(String.format("Average number of " +
				"dependencies per class: %.1f (%.1f%% of all classes)",
				averageDependenciesPerClass, percentage));
	}

	private static Collection<File> getAllJavaFiles(String[] dirs) {
		Collection<File> javaFiles = new ArrayList<File>();
		for (String dir : dirs) {
			javaFiles.addAll(FileUtils.listFiles(
					new File(dir), new String[] {"java"}, true));
		}
		return javaFiles;
	}

	public static Collection<JDClass> getAllDependencies(
			JDClass jdClass, Collection<JDClass> allClasses) {
		Collection<JDClass> dependencies = new ArrayList<JDClass>();
		addAllDependencies(dependencies, jdClass, allClasses, jdClass);
		return dependencies;
	}

	private static void addAllDependencies(
			Collection<JDClass> dependencies, JDClass jdClass,
			Collection<JDClass> allClasses, JDClass root) {
		Map<String, JDClass> classPool = getClassPool(allClasses);
		Collection<JDClass> immediateDependencies =
			jdClass.getImmediateDependencies(allClasses);
		for (JDClass immediateDependency : immediateDependencies) {
			String key = immediateDependency.getFullName();
			if (!classPool.containsKey(key)) {
				throw new IllegalArgumentException("Class " +
						immediateDependency + " not found in class pool.");
			}
			JDClass cachedDependency = classPool.get(key);
			if (!dependencies.contains(cachedDependency)
					&& !root.equals(cachedDependency)) {
				dependencies.add(cachedDependency);
				addAllDependencies(dependencies, cachedDependency, allClasses, root);
			}
		}
	}

	private static Map<String, JDClass> getClassPool(
			Collection<JDClass> allClasses) {
		Map<String, JDClass> classPool = new HashMap<String, JDClass>();
		for (JDClass clazz : allClasses) {
			classPool.put(clazz.getFullName(), clazz);
		}
		return classPool;
	}
}
