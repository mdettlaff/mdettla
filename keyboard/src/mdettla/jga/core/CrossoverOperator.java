package mdettla.jga.core;

import java.util.List;

/**
 * Operator krzyżowania.
 */
public interface CrossoverOperator {

	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2);
}
