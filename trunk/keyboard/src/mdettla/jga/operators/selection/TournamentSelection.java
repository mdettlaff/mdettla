package mdettla.jga.operators.selection;

import java.util.Collections;
import java.util.List;

import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;

/**
 * Selekcja turniejowa (podwójna). W podwójnej selekcji turniejowej wybiera
 * się najpierw grupę złożoną z {@code k} osobników, a następnie z grupy
 * tej wybiera się najlepiej dostosowanego osobnika.
 */
public class TournamentSelection extends AbstractTournamentSelection {

	public TournamentSelection() {
	}

	public TournamentSelection(int tournamentSize) {
		super(tournamentSize);
	}

	@Override
	public Specimen select(List<Specimen> population) {
		return Collections.max(Utils.randomSample(population, tournamentSize));
	}
}
