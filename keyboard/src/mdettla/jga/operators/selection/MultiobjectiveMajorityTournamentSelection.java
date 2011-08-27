package mdettla.jga.operators.selection;

import java.util.List;

import mdettla.jga.core.Specimen;
import mdettla.jga.core.Utils;
import mdettla.keyboard.ga.KeyboardLayout;

public class MultiobjectiveMajorityTournamentSelection extends AbstractTournamentSelection {

	public MultiobjectiveMajorityTournamentSelection() {
	}

	public MultiobjectiveMajorityTournamentSelection(int tournamentSize) {
		super(tournamentSize);
	}

	@Override
	public Specimen select(List<Specimen> population) {
		List<Specimen> tournament = Utils.randomSample(population, tournamentSize);
		Specimen best = tournament.get(0);
		for (Specimen specimen : tournament) {
			int c = 0;
			for (int i = 0; i < ((KeyboardLayout)specimen).OBJECTIVES.length; i++) {
				double objectiveValue = ((KeyboardLayout)specimen).OBJECTIVES[i].getValue();
				double bestObjectiveValue = ((KeyboardLayout)best).OBJECTIVES[i].getValue();
				if (objectiveValue < bestObjectiveValue) {
					c += 1;
				} else if (objectiveValue > bestObjectiveValue) {
					c -= 1;
				}
			}
			if (c > 0 || (c == 0 && specimen.compareTo(best) > 0)) {
				best = specimen;
			}
		}
		return best;
	}
}
