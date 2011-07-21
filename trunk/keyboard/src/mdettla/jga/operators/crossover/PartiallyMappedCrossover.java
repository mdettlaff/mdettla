package mdettla.jga.operators.crossover;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.Specimen;

public class PartiallyMappedCrossover implements CrossoverOperator {

	private final Random random = new Random();

	@Override
	public List<Specimen> produceOffspring(Specimen parent1, Specimen parent2) {
		int genotypeLength = parent1.getGenotypeLength();
		int[] cutPoints = {createCutPoint(genotypeLength), createCutPoint(genotypeLength)};
		Arrays.sort(cutPoints);
		return produceOffspring(parent1, parent2, cutPoints[0], cutPoints[1]);
	}

	private int createCutPoint(int genotypeLength) {
		return random.nextInt(genotypeLength + 1);
	}

	List<Specimen> produceOffspring(
			Specimen parent1, Specimen parent2, int leftCutPoint, int rightCutPoint) {
		Specimen offspring1 = createSingleOffspring(
				parent1, parent2, leftCutPoint, rightCutPoint);
		Specimen offspring2 = createSingleOffspring(
				parent2, parent1, leftCutPoint, rightCutPoint);
		return Arrays.asList(offspring1, offspring2);
	}

	private Specimen createSingleOffspring(Specimen parent, Specimen otherParent,
			int leftCutPoint, int rightCutPoint) {
		int genotypeLength = parent.getGenotypeLength();
		List<Object> offspring1Genotype = new ArrayList<Object>();
		// copying mapping sections
		for (int i = 0; i < genotypeLength; i++) {
			boolean isInsideMappingSection = i >= leftCutPoint && i < rightCutPoint;
			Object gene = isInsideMappingSection ? otherParent.getGeneAt(i) : null;
			offspring1Genotype.add(gene);
		}
		// create mappings
		BidirectionalMap mappings = new BidirectionalMap();
		for (int i = leftCutPoint; i < rightCutPoint; i++) {
			mappings.put(offspring1Genotype.get(i), parent.getGeneAt(i));
		}
		// assigning outside of mapping sections according to mappings
		for (int i = 0; i < genotypeLength; i++) {
			boolean isInsideMappingSection = i >= leftCutPoint && i < rightCutPoint;
			if (!isInsideMappingSection) {
				Object substitution = parent.getGeneAt(i);
				while (offspring1Genotype.contains(substitution)) {
					substitution = mappings.getAndRemove(substitution);
				}
				offspring1Genotype.set(i, substitution);
			}
		}
		Specimen offspring1 = createSpecimen(offspring1Genotype, parent);
		return offspring1;
	}

	private Specimen createSpecimen(List<Object> genotype, Specimen parent) {
		Specimen specimen = parent.createCopy();
		for (int i = 0; i < parent.getGenotypeLength(); i++) {
			specimen.setGeneAt(i, genotype.get(i));
		}
		return specimen;
	}


	private static class BidirectionalMap {

		private Map<Object, Object> keys1 = new LinkedHashMap<Object, Object>();
		private Map<Object, Object> keys2 = new LinkedHashMap<Object, Object>();

		public void put(Object key1, Object key2) {
			keys1.put(key1, key2);
			keys2.put(key2, key1);
		}

		public Object getAndRemove(Object key) {
			if (keys1.containsKey(key)) {
				Object value = keys1.get(key);
				keys1.remove(key);
				keys2.remove(value);
				return value;
			} else if (keys2.containsKey(key)) {
				Object value = keys2.get(key);
				keys2.remove(key);
				keys1.remove(value);
				return value;
			}
			return new IllegalArgumentException("Key " + key + " not found.");
		}
	}
}
