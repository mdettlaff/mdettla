package mdettla.ea.zadanie2;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;

public class RunEvolutionaryAlgorithm {

	private static final int EPOCHS_COUNT = 30;

	private static double averageFitness(List<Specimen> population) {
		double sum = 0;
		for (Specimen specimen : population) {
			sum += specimen.getFitness();
		}
		return sum / population.size();
	}

	private static EvolutionaryAlgorithm constructEvolutionaryAlgorithm(
			String algorithmName, String fitnessFunctionName,
			int varsCount, int generationsCount, int populationSize) {
		EvolutionaryAlgorithm ea = null;
		try {
			FitnessFunction fitnessFunction =
				(FitnessFunction)Class.forName(fitnessFunctionName).newInstance();

			Class[] constructorArgsClasses = new Class[] {
				FitnessFunction.class, int.class, int.class, int.class
			};
			Object[] constructorArguments = new Object[] {
				fitnessFunction, varsCount, generationsCount, populationSize
			};
			ea = (EvolutionaryAlgorithm)Class.forName(algorithmName)
				.getConstructor(constructorArgsClasses)
				.newInstance(constructorArguments);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		} catch (InstantiationException e) {
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			e.printStackTrace();
		} catch (NoSuchMethodException e) {
			e.printStackTrace();
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		}
		return ea;
	}

	public static void main(String[] args) {
		if (args.length < 3) {
			System.out.println("Użycie: java " +
					RunEvolutionaryAlgorithm.class.getName()
					+ " KLASA_ALGORYTMU_EWOLUCYJNEGO"
					+ " LICZBA_ITERACJI"
					+ " KLASA_FUNKCJI_PRZYSTOSOWANIA");
		} else {
			int varsCount = 30;
			int populationSize = 15;
			int generationsCount = Integer.parseInt(args[1]);

			EvolutionaryAlgorithm evolutionaryAlgorithm =
				constructEvolutionaryAlgorithm(args[0], args[2],
						varsCount, generationsCount, populationSize);

			/*
			 * Obliczamy przystosowanie najlepszego osobnika w kolejnych
			 * generacjach uśrednione po 30 powtórzeniach algorytmu.
			 */
			List<List<Specimen>> bestInGenLists =
				new ArrayList<List<Specimen>>();
			for (int i = 0; i < generationsCount; i++) {
				bestInGenLists.add(new ArrayList<Specimen>());
			}
			for (int i = 0; i < EPOCHS_COUNT; i++) {
				// uruchamiamy algorytm ewolucyjny
				List<Specimen> bestInEpoch =
					evolutionaryAlgorithm.runAlgorithm();
				for (int j = 0; j < bestInEpoch.size(); j++) {
					bestInGenLists.get(j).add(bestInEpoch.get(j));
				}
				System.err.print('.');
			}
			System.err.println();
			for (int i = 0; i < bestInGenLists.size(); i++) {
				System.out.println(String.format("%d\t%.5f", i + 1,
							averageFitness(bestInGenLists.get(i))));
			}
		}
	}
}
