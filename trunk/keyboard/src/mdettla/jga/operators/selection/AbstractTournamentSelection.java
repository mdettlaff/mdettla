package mdettla.jga.operators.selection;

import mdettla.jga.core.SelectionFunction;

public abstract class AbstractTournamentSelection implements SelectionFunction {

	protected int tournamentSize;

	public AbstractTournamentSelection() {
		this.tournamentSize = 2;
	}

	public AbstractTournamentSelection(int tournamentSize) {
		this.tournamentSize = tournamentSize;
	}

	public void setTournamentSize(int tournamentSize) {
		this.tournamentSize = tournamentSize;
	}
}
