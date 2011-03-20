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

    public static void main(String[] args) throws IOException {
        Collection<JDClass> allClasses = new ArrayList<JDClass>();
        Collection<File> allJavaFiles = getAllJavaFiles();
        if (allJavaFiles.size() > 0) {
        	for (File javaFile : allJavaFiles) {
        		String javaSourceCode = FileUtils.readFileToString(javaFile);
        		allClasses.add(JDClass.fromJavaSourceCode(javaSourceCode));
        	}
        	int allDependenciesCount = 0;
        	for (JDClass jdClass : allClasses) {
        		int dependenciesForClassCount =
        			getAllDependencies(jdClass, allClasses).size();
        		allDependenciesCount += dependenciesForClassCount;
        		System.out.println(jdClass.getFullName() + ": " +
        				dependenciesForClassCount);
        	}
            double averageDependenciesPerClass =
            	((double)allDependenciesCount) / allJavaFiles.size();
            System.out.println("Average dependencies per class: " +
            		averageDependenciesPerClass);
        } else {
            System.err.println("No classes found.");
        }
    }

    private static Collection<File> getAllJavaFiles() {
		return FileUtils.listFiles(new File("src"), new String[] {"java"}, true);
	}

	public static Collection<JDClass> getAllDependencies(
    		JDClass jdClass, Collection<JDClass> allClasses) {
		Collection<JDClass> dependencies = new ArrayList<JDClass>();
		addAllDependencies(dependencies, jdClass, allClasses, jdClass);
//		Map<String, JDClass> classPool = getClassPool(allClasses);
//		Collection<JDClass> dependencies = new ArrayList<JDClass>();
//		Collection<JDClass> immediateDependencies =
//			jdClass.getImmediateDependencies(allClasses);
//		for (JDClass immediateDependency : immediateDependencies) {
//			String key = immediateDependency.getFullName();
//			if (!classPool.containsKey(key)) {
//				throw new IllegalArgumentException("Class " +
//						immediateDependency + " not found in class pool.");
//			}
//			JDClass cachedDependency = classPool.get(key);
//			if (!dependencies.contains(cachedDependency)
//					&& !jdClass.equals(cachedDependency)) {
//				dependencies.add(cachedDependency);
//				dependencies.addAll(
//						getAllDependencies(cachedDependency, allClasses));
//			}
//		}
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
