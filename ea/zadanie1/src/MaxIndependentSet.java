import java.util.Iterator;
import java.util.HashSet;
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
		BufferedReader in =
			new BufferedReader(new InputStreamReader(System.in));
		Graph<Integer> graph = Graph.readGraph(in);

		System.out.println("Maksymalny zbiór niezależny w podanym grafie:");
		System.out.println(maxIndependentSet(graph));
	}

	/**
	 * Znajduje maksymalny zbiór niezależny w podanym grafie.
	 */
	public static <T> Set<T> maxIndependentSet(Graph<T> graph) {
		Set<T> mis = new HashSet<T>();
		Set<T> vertices = graph.getVertices();
		T currentVertex = vertices.iterator().next();
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
