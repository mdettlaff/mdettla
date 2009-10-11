package mdettla.jga.operators.selection;

import java.util.Collections;
import java.util.List;

import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

public class TournamentSelection implements SelectionFunction {

	private int tournamentSize;

	public TournamentSelection(int tournamentSize) {
		this.tournamentSize = tournamentSize;
	}

	@Override
	public Specimen select(List<Specimen> population) {
		return Collections.max(Utils.randomSample(population, tournamentSize));
	}
}
