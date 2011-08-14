package mdettla.jga.core;


/**
 * Pojedynczy osobnik. Populacje w algorytmie genetycznym muszą składać się
 * z instancji klas implementujących ten interfejs. Osobniki są porównywalne
 * ze względu na wartość przystosowania za pomocą metody {@code compareTo};
 * większa wartość funkcji przystosowania może oznaczać lepsze bądź gorsze
 * przystosowanie, w zależności od rozwiązywanego problemu.
 */
public interface Specimen extends Comparable<Specimen> {

	Specimen createCopy();

	int getGenotypeLength();

	Object getGeneAt(int position);

	void setGeneAt(int position, Object gene);

	/**
	 * Ustawia losową wartość genu na danej pozycji w genotypie.
	 */
	void setRandomGeneValueAt(int position);

	/**
	 * Ustawia przeciwną wartość genu na danej pozycji w genotypie.
	 * Na przykład, w reprezentacji binarnej zamienia wartość {@literal 1}
	 * na {@literal 0} i odwrotnie.
	 */
	void setOppositeGeneValueAt(int position);

	void computeFitness();

	/**
	 * Wartość przystosowania tego osobnika.
	 *
	 * @return Liczba reprezentująca przystosowanie.
	 */
	Number getFitness();

	String getPhenotype();
}
