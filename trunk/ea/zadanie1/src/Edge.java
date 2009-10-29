/**
 * Krawędź w grafie, reprezentowana przez parę wierzchołków.
 */
public class Edge<T> {

	/**
	 * Pierwszy wierzchołek.
	 */
	public final T V1;
	/**
	 * Drugi wierzchołek.
	 */
	public final T V2;

	public Edge(T V1, T V2) {
		this.V1 = V1;
		this.V2 = V2;
	}
}
