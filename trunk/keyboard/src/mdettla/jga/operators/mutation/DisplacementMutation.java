package mdettla.jga.operators.mutation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.Specimen;

public class DisplacementMutation implements MutationOperator {

	private final Random random = new Random();

	@Override
	public void mutate(Specimen specimen) {
		int genotypeLength = specimen.getGenotypeLength();
		int[] cutPoints = {createCutPoint(genotypeLength), createCutPoint(genotypeLength)};
		Arrays.sort(cutPoints);
		int insertionPoint = random.nextInt(genotypeLength + 1 - (cutPoints[1] - cutPoints[0]));
		mutate(specimen, cutPoints[0], cutPoints[1], insertionPoint);
	}

	void mutate(Specimen specimen,
			int cutPointLeft, int cutPointRight, int insertionPoint) {
		List<Object> genes = getGenes(specimen);
		List<Object> subList = genes.subList(cutPointLeft, cutPointRight);
		List<Object> result = new ArrayList<Object>();
		result.addAll(genes.subList(0, cutPointLeft));
		result.addAll(genes.subList(cutPointRight, genes.size()));
		result.addAll(insertionPoint, subList);
		setGenes(specimen, result);
	}

	private int createCutPoint(int genotypeLength) {
		return random.nextInt(genotypeLength + 1);
	}

	private void setGenes(Specimen specimen, List<Object> genes) {
		for (int i = 0; i < genes.size(); i++) {
			specimen.setGeneAt(i, genes.get(i));
		}
	}

	private List<Object> getGenes(Specimen specimen) {
		List<Object> genes = new ArrayList<Object>();
		for (int i = 0; i < specimen.getGenotypeLength(); i++) {
			genes.add(specimen.getGeneAt(i));
		}
		return genes;
	}
}
