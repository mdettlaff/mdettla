package mdettla.jga.core;

import java.util.Arrays;
import java.util.List;
import java.util.Random;

class SpecimenCreatorThread extends Thread {
	private GeneticAlgorithm ga;
	private int specimensToCreate;
	private List<Specimen> originalPopulation;
	private List<Specimen> population;
	Random random;

	public SpecimenCreatorThread(GeneticAlgorithm ga, int specimens,
			List<Specimen> population, List<Specimen> originalPopulation) {
		super();
		this.ga = ga;
		this.specimensToCreate = specimens;
		this.population = population;
		this.originalPopulation = originalPopulation;
		random = new Random();
	}

	public void run() {
		int specimensCreated = 0;
		while (specimensCreated < specimensToCreate) {
			SelectionFunction selectionFunction = ga.getSelectionFunction();
			Specimen parent1 = selectionFunction.select(originalPopulation);
			Specimen parent2 = selectionFunction.select(originalPopulation);
			List<Specimen> offspring;
			if (random.nextDouble() < ga.getCrossoverProbability()) {
				offspring = ga.getCrossoverOperator().produceOffspring(parent1, parent2);
			} else {
				offspring = Arrays.asList(parent1, parent2);
			}
			for (Specimen specimen : offspring) {
				if (random.nextDouble() < ga.getMutationProbability()) {
					ga.getMutationOperator().mutate(specimen);
				}
				population.add(specimen);
				specimensCreated++;
				if (specimensCreated == specimensToCreate) {
					break;
				}
			}
		}
	}
}
