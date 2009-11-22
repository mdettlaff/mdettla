import java.util.Iterator;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.IOException;

/**
 * Znajdowanie największego zbioru niezależnego w grafie za pomocą
 * prostego algorytmu.
 *
 * Graf należy podać ze standardowego wejścia w formacie DIMACS Graph.
 */
public class MaxIndependentSet {

	public static void main(String[] args) throws IOException {
		int repeats = 30;

		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		Graph<Integer> graph = Graph.readGraph(in);

		long timeBegin = System.currentTimeMillis();
		System.out.println(
				"Znalezione maksymalne zbiory niezależne w podanym grafie:");
		for (int i = 0; i < repeats - 1; i++) {
			System.out.println(maxIndependentSet(graph));
		}
		long timeEnd = System.currentTimeMillis();
		System.out.println("Czas wykonania: " + (timeEnd - timeBegin) + " ms");
	}

	/**
	 * Znajduje maksymalny zbiór niezależny w podanym grafie.
	 */
	public static <T> Set<T> maxIndependentSet(Graph<T> graph) {
		Random random = new Random();
		Set<T> mis = new HashSet<T>();
		Set<T> vertices = new HashSet<T>(graph.getVertices());
		T currentVertex = null;
		Iterator<T> it = vertices.iterator();
		for (int i = 0; i < random.nextInt(vertices.size()) + 1; i++) {
			currentVertex = it.next();
		}
		vertices.remove(currentVertex);
		mis.add(currentVertex);
		while (!vertices.isEmpty()) {
			T found = null;
			for (T candidate : vertices) {
				boolean isCandidateGood = true;
				for (T v : mis) {
					if (graph.areVerticesAdjacent(v, candidate)) {
						isCandidateGood = false;
					}
				}
				if (isCandidateGood) {
					found = candidate;
					break;
				}
			}
			if (found != null) {
				currentVertex = found;
				vertices.remove(found);
				mis.add(currentVertex);
			} else {
				break;
			}
		}
		return mis;
	}
}
