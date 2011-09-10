package mdettla.jga.core;

import java.util.ArrayList;
import java.util.List;

public class ConcurrentGeneticAlgorithm extends GeneticAlgorithm {

	public static final int DEFAULT_THREAD_POOL_SIZE = 7;

	private int threadPoolSize;

	public ConcurrentGeneticAlgorithm(List<Specimen> initialPopulation) {
		super(initialPopulation);
		threadPoolSize = DEFAULT_THREAD_POOL_SIZE;
	}

	public void setThreadPoolSize(int threadPoolSize) {
		if (threadPoolSize < 1) {
			throw new IllegalArgumentException(
					"Argument threadPoolSize must be greater than 0.");
		}
		this.threadPoolSize = threadPoolSize;
	}

	@Override
	public void computeFitness(List<Specimen> population) {
		try {
			List<Thread> threadPool = new ArrayList<Thread>();
			List<List<Specimen>> subPopulations =
				Utils.partition(population, threadPoolSize);
			for (List<Specimen> subPopulation : subPopulations) {
				Thread thread = new FitnessComputationThread(subPopulation);
				thread.start();
				threadPool.add(thread);
			}
			for (int j = 0; j < threadPool.size(); j++) {
				threadPool.get(j).join();
			}
		} catch (InterruptedException e) {
			throw new RuntimeException(e);
		}
	}

	private static class FitnessComputationThread extends Thread {

		private List<Specimen> population;

		public FitnessComputationThread(List<Specimen> population) {
			this.population = population;
		}

		@Override
		public void run() {
			for (Specimen specimen : population) {
				specimen.computeFitness();
			}
		}
	}
}
