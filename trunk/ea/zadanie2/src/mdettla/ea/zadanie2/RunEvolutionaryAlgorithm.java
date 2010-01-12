package mdettla.ea.zadanie2;

import java.lang.reflect.InvocationTargetException;

public class RunEvolutionaryAlgorithm {

	public static void main(String[] args) {
		try {
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

				FitnessFunction fitnessFunction =
					(FitnessFunction)Class.forName(args[2]).newInstance();

				Class[] constructorArgsClasses = new Class[] {
					FitnessFunction.class, int.class, int.class, int.class
				};
				Object[] constructorArguments = new Object[] {
					fitnessFunction, varsCount, generationsCount, populationSize
				};
				EvolutionaryAlgorithm evolutionaryAlgorithm =
					(EvolutionaryAlgorithm)Class.forName(args[0]).
					getConstructor(constructorArgsClasses).newInstance(
							constructorArguments);

				evolutionaryAlgorithm.runAlgorithm();
			}
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
	}
}
