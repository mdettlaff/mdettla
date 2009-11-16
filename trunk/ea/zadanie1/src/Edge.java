/**
 * Krawędź w grafie nieskierowanym, reprezentowana przez parę wierzchołków.
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

	public boolean contains(T v) {
		return V1.equals(v) || V2.equals(v);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj instanceof Edge) {
			Edge other = (Edge)obj;
			return (this.V1.equals(other.V1) && this.V2.equals(other.V2))
				|| (this.V1.equals(other.V2) && this.V2.equals(other.V1));
		}
		return false;
	}
}
