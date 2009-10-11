package mdettla.jga.core;

import java.util.List;

public interface CrossoverOperator {

	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2);
}
