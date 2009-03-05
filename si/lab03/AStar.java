import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

/**
 * Przykładowa implementacja algorytmu A*.
 *
 * Wynik działania algorytmu: najkrótsza ścieżka w grafie
 * łącząca węzeł początkowy z węzłem końcowym.
 *
 * Użycie: java AStar graph_file heuristic_file target_node_name
 */

public class AStar {
	/**
	 * Graf przechowujący miasta, odległości między nimi i heurystykę.
	 * Kluczami są nazwy miast.
	 */
	private static Map<String, Node> graph = new HashMap<String, Node>();
	/**
	 * Nazwa węzła początkowego.
	 */
	private static String w0 = "";
	/**
	 * Nazwa węzła końcowego.
	 */
	private static String wg;
	/**
	 * Lista węzłów do rozwinięcia.
	 */
	private static List<Node> open = new LinkedList<Node>();
	/**
	 * Lista zbadanych węzłów.
	 */
	private static List<Node> closed = new LinkedList<Node>();

	private AStar() {};

	public static void main(String[] args) {
		try {
			readInputData(args);
			System.out.println(graph);
		} catch (IOException e) {
			System.out.println("Nie można odczytać podanego pliku.");
		}
	}

	private static void readInputData(String[] args) throws IOException {
			BufferedReader reader = new BufferedReader(new FileReader(
						args[0]));
			String inputLine;

			while ((inputLine = reader.readLine()) != null) {
				String[] nodeInfo = inputLine.split(" ");
				if (!graph.containsKey(nodeInfo[0])) {
					Node node = new Node(nodeInfo[0]);
					node.getNeighbors().put(nodeInfo[1], 
							Double.parseDouble(nodeInfo[2]));
					graph.put(nodeInfo[0], node);
				} else {
					graph.get(nodeInfo[0]).getNeighbors().put(nodeInfo[1], 
							Double.parseDouble(nodeInfo[2]));
				}
			}

			reader = new BufferedReader(new FileReader(args[1]));
			while ((inputLine = reader.readLine()) != null) {
				String[] nodeInfo = inputLine.split(" ");
				if (Integer.parseInt(nodeInfo[1]) == 0) {
					w0 = nodeInfo[0];
				}
				if (!graph.containsKey(nodeInfo[0])) {
					graph.put(nodeInfo[0], new Node(nodeInfo[0]));
				}
				graph.get(nodeInfo[0]).setH(
						Double.parseDouble(nodeInfo[1]));
			}

			wg = args[2];
	}
}


/**
 * Węzeł w grafie.
 */
class Node {
	private String name;
	/**
	 * Sąsiedzi i przypisane im koszty.
	 */
	private Map<String, Double> neighbors = new HashMap<String, Double>();
	/**
	 * Skąd przyszliśmy do tego węzła.
	 */
	private Node previous;
	/**
	 * Heurystyka.
	 */
	private Double h;

	public Node(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}

	public Map<String, Double> getNeighbors() {
		return neighbors;
	}
	public void setNeighbors(Map<String, Double> neighbors) {
		this.neighbors = neighbors;
	}

	public Node getPrevious() {
		return previous;
	}
	public void setPrevious(Node previous) {
		this.previous = previous;
	}

	public Double getH() {
		return h;
	}
	public void setH(Double h) {
		this.h = h;
	}

	public String toString() {
		return name + ": " + neighbors + ", prev: " + previous + ", h: " + h;
	}
}
