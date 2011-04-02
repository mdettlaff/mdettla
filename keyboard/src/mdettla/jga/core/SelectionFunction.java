package mdettla.jga.core;

import java.util.List;

/**
 * Funkcja selekcji.
 */
public interface SelectionFunction {

	/**
	 * Wybiera osobnika z populacji, biorąc pod uwagę wartość przystosowania
	 * osobników w populacji (osobniki o większym przystosowaniu powinny
	 * mieć większą szansę na zostanie wybranymi).
	 */
	public Specimen select(List<Specimen> population);
}
