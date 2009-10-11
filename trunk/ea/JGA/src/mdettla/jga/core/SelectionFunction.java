package mdettla.jga.core;

import java.util.List;

public interface SelectionFunction {

	public Specimen select(List<Specimen> population);
}
