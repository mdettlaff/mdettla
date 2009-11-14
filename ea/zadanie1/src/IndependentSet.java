import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Random;
import java.util.Set;

import mdettla.jga.core.Specimen;

/**
 * Zbiór niezależny dla określonego grafu.
 */
public class IndependentSet implements Specimen {

	/*
	 * Graf na którym określony jest zbiór niezależny.
	 */
	private static List<Integer> vertices;
	private static Set<Edge<Integer>> edges;

	private static final int GENOTYPE_LENGTH = 24;
	/**
	 * Reprezentacja zbioru niezależnego. Wartości boolowskie określają, czy
	 * na danej pozycji listy wierzchołków <code>vertices</code> znajduje się
	 * wierzchołek należący do zbioru niezależnego.
	 */
	private List<Boolean> genotype;

	public IndependentSet(List<Boolean> genotype) {
		if (vertices == null || edges == null) {
			throw new IllegalStateException("Najpierw zainicjuj graf, " +
					"na którym określony zostanie zbiór niezależny " +
					"(metoda setGraph).");
		}
		this.genotype = genotype;
	}

	/**
	 * Definiuje graf, na którym określony jest zbiór niezależny.
	 */
	public static void setGraph(List<Integer> V, Set<Edge<Integer>> E) {
		vertices = V;
		edges = E;
	}

	public static Specimen createRandomInstance() {
		Random random = new Random();
		List<Boolean> randomGenotype = new ArrayList<Boolean>(GENOTYPE_LENGTH);
		for (int i = 0; i < GENOTYPE_LENGTH; i++) {
			randomGenotype.add(random.nextBoolean());
		}
		return new IndependentSet(randomGenotype);
	}

	@Override
	public Specimen createCopy() {
		return new IndependentSet(new ArrayList<Boolean>(genotype));
	}

	@Override
	public int getGenotypeLength() {
		return GENOTYPE_LENGTH;
	}

	@Override
	public Object getGeneAt(int position) {
		return genotype.get(position);
	}

	@Override
	public void setGeneAt(int position, Object gene) {
		genotype.set(position, (Boolean)gene);
	}

	@Override
	public void setRandomGeneValueAt(int position) {
		Random random = new Random();
		genotype.set(position, random.nextBoolean());
	}

	@Override
	public void setOppositeGeneValueAt(int position) {
		genotype.set(position, !genotype.get(position));
	}

	@Override
	public Number getFitness() {
		int independentSetSize = 0;
		for (boolean gene : genotype) {
			independentSetSize += gene ? 1 : 0;
		}
		int punishment = 0;
		Set<Integer> content = getContent();
		for (Edge<Integer> edge : edges) {
			if (content.contains(edge.V1) && content.contains(edge.V2)) {
				punishment++;
			}
		}
		return independentSetSize - getGenotypeLength() * punishment;
	}

	@Override
	public int compareTo(Specimen other) {
		if (getFitness().intValue() > other.getFitness().intValue()) {
			return 1;
		} else if (getFitness().intValue() < other.getFitness().intValue()) {
			return -1;
		} else {
			return 0;
		}
	}

	/**
	 * Zwraca zbiór wierzchołków należących do tego zbioru niezależnego.
	 */
	public Set<Integer> getContent() {
		Set<Integer> content = new HashSet<Integer>();
		for (int i = 0; i < getGenotypeLength(); i++) {
			if (genotype.get(i)) {
				content.add(vertices.get(i));
			}
		}
		return content;
	}
}
