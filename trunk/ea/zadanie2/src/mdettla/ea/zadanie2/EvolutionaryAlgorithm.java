package mdettla.ea.zadanie2;

import java.util.List;

public interface EvolutionaryAlgorithm {

	/**
	 * Uruchamia jeden przebiej (epokę) algorytmu genetycznego.
	 *
	 * @return Lista najlepszych osobników w kolejnych generacjach.
	 */
	public List<Specimen> runAlgorithm();
}
