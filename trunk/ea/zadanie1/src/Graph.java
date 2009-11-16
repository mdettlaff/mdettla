import java.util.Set;
import java.util.HashSet;
import java.io.BufferedReader;
import java.io.IOException;

import mdettla.jga.core.Utils;

/**
 * Graf.
 */
public class Graph<T> {
	private Set<T> vertices;
	private Set<Edge<T>> edges;

	public Graph(Set<T> vertices, Set<Edge<T>> edges) {
		this.vertices = vertices;
		this.edges = edges;
	}

	public Set<T> getVertices() {
		return vertices;
	}

	public Set<Edge<T>> getEdges() {
		return edges;
	}

	public boolean areVerticesAdjacent(T v1, T v2) {
		for (Edge<T> edge : edges) {
			if (edge.contains(v1) && edge.contains(v2)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Tworzy graf czytając go z podanego wejścia w formacie DIMACS Graph.
	 */
	public static Graph<Integer> readGraph(BufferedReader in)
		throws IOException {
		// czytamy ilość wierzchołków
		String line = in.readLine();
		int verticesCount = Integer.valueOf(line.split(" ")[2]);
		Set<Integer> vertices =
			new HashSet<Integer>(Utils.range(1, verticesCount));
		// czytamy krawędzie
		int edgesCount = Integer.valueOf(line.split(" ")[3]);
		Set<Edge<Integer>> edges = new HashSet<Edge<Integer>>();
		for (int i = 0; i < edgesCount; i++) {
			line = in.readLine();
			String[] lineValues = line.split(" ");
			int v1 = Integer.valueOf(lineValues[1]);
			int v2 = Integer.valueOf(lineValues[2]);
			edges.add(new Edge<Integer>(v1, v2));
		}
		return new Graph<Integer>(vertices, edges);
	}
}
