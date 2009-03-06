import java.util.List;
import java.util.LinkedList;
import java.util.Map;
import java.util.HashMap;
import java.util.Iterator;
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
import java.io.IOException;

/**
 * Implementacja algorytmu A*.
 *
 * Wynik działania algorytmu: najkrótsza ścieżka w grafie
 * łącząca węzeł początkowy z węzłem końcowym.
 *
 * Użycie: java AStar plik_grafu plik_heurezy nazwa_węzła_początkowego
 *
 * Za węzeł końcowy uznawany jest węzeł o heurezie równej 0.
 *
 * @author Michał Dettlaff
 */

public class AStar {
	/**
	 * Graf skierowany przechowujący miasta, odległości między nimi
	 * i heurystykę. Kluczami są nazwy miast.
	 */
	private static Map<String, Node> graph = new HashMap<String, Node>();
	/**
	 * Węzeł początkowy.
	 */
	private static Node w0;
	/**
	 * Węzeł końcowy.
	 */
	private static Node wg;
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
		if (args.length > 2) {
			try {
				LinkedList<Node> path;

				readInputData(args);

				path = AStar(w0, wg);

				if (path != null) {
					System.out.println("Najkrótsza ścieżka od " +
							w0.getName() + " do " + wg.getName() + ":");
					System.out.print(w0.getName());
					Iterator<Node> iterator = path.descendingIterator();
					while (iterator.hasNext()) {
						System.out.print(" -> " + iterator.next().getName());
					}
					System.out.println();
				} else {
					System.out.println("Nie można znaleźć ścieżki.");
				}
			} catch (IOException e) {
				System.out.println("Nie można odczytać podanego pliku.");
				System.exit(1);
			}
		} else {
			System.out.println(
					"Implementacja algorytmu A*.\n" +
					"Wynik działania algorytmu: najkrótsza ścieżka w grafie\n" +
					"łącząca węzeł początkowy z węzłem końcowym.\n\n" +
					"Użycie: java AStar plik_grafu plik_heurezy " +
					"nazwa_węzła_początkowego\n" +
					"Za węzeł końcowy uznawany jest węzeł o heurezie równej 0."
					);
		}
	}

	/**
	 * Algorytm A*. Znajduje najkrótszą ścieżkę od węzła w0 do węzła wg.
	 *
	 * @return Najkrótsza ścieżka pomiędzy podanymi węzłami. Jeśli nie
	 *         znaleziono ścieżki, zwraca null.
	 */
	public static LinkedList<Node> AStar(Node w0, Node wg) {
		open.add(w0);
		w0.setG(0.0);
		w0.setF(w0.getH());
		while (!open.isEmpty()) {
			// z listy open wybierz węzeł w o najniższym koszcie f(w)
			Node node = minimum(open);
			if (node.equals(wg)) {
				return node.path();
			}
			open.remove(node);
			closed.add(node);
			/*
			 * oceń sąsiadów węzła w, tzn. dla każdego v ∈ N(w) wyznacz
			 * g(v) = g(w) + κ(w, v), oblicz h(v) oraz f(v) = g(v) + h(v);
			 * dodaj do listy open wszystkich nieodwiedzonych sąsiadów
			 * węzła w, którzy albo nie występują w open, albo są już w open,
			 * ale przypisany im koszt jest większy od kosztu aktualnie
			 * wyznaczonego
			 */
			for (String neighborName : node.getNeighbors().keySet()) {
				Node neighbor = graph.get(neighborName);
				if (closed.contains(neighbor)) {
					continue;
				}
				Double currentG = node.getG() +
					node.getNeighbors().get(neighbor.getName());
				boolean currentIsBetter = false;
				if (!open.contains(neighbor)) {
					open.add(neighbor);
					currentIsBetter = true;
				} else if (currentG < neighbor.getG()) {
					currentIsBetter = true;
				}
				if (currentIsBetter) {
					neighbor.setPrevious(node);
					neighbor.setG(currentG);
					neighbor.setF(neighbor.getG() + neighbor.getH());
				}
			}
		}
		return null;
	}

	/**
	 * Zwraca węzeł o najmniejszym koszcie z listy.
	 */
	private static Node minimum(List<Node> nodes) {
		Node minimum = nodes.get(0);
		for (Node node : nodes) {
			if (node.getF() < minimum.getF()) {
				minimum = node;
			}
		}
		return minimum;
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
				wg = graph.get(nodeInfo[0]);
			}
			if (!graph.containsKey(nodeInfo[0])) {
				graph.put(nodeInfo[0], new Node(nodeInfo[0]));
			}
			graph.get(nodeInfo[0]).setH(
					Double.parseDouble(nodeInfo[1]));
		}

		w0 = graph.get(args[2]);
		if (!graph.containsValue(w0)) {
			System.out.println("Podany węzeł początkowy nie istnieje");
			System.exit(1);
		}
	}
}


/**
 * Węzeł w grafie.
 */
class Node {
	/**
	 * Nazwa identyfikująca węzeł (nazwa miasta).
	 */
	private String name;
	/**
	 * Sąsiedzi (ich nazwy) i przypisane im koszty.
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
	/**
	 * Koszt ścieżki od w0 do tego węzła.
	 */
	private Double g;
	/**
	 * Funkcja kosztu.
	 */
	private Double f;

	public Node(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
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

	public Double getG() {
		return g;
	}
	public void setG(Double g) {
		this.g = g;
	}

	public Double getF() {
		return f;
	}
	public void setF(Double f) {
		this.f = f;
	}


	public String toString() {
		String previousName;
		if (previous != null) {
			previousName = previous.getName();
		} else {
			previousName = null;
		}
		return name + ": " + neighbors + ", prev: " + previousName +
			", h: " + h;
	}

	/**
	 * Zwraca ścieżkę od tego węzła do w0.
	 */
	public LinkedList<Node> path() {
		LinkedList<Node> path = new LinkedList<Node>();
		Node w = this;
		while (w.getPrevious() != null) { // previous == null dla w0
			path.add(w);
			w = w.getPrevious();
		}
		return path;
	}

	public boolean equals(Object obj) {
		Node node = (Node) obj;
		return this.name.equals(node.getName());
	}
}
