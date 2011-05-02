package mdettla.jga.core;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ConcurrentGeneticAlgorithm extends GeneticAlgorithm {

	public static final int DEFAULT_THREAD_POOL_SIZE = 7;

	private int threadPoolSize;

	public ConcurrentGeneticAlgorithm(List<Specimen> initialPopulation) {
		super(initialPopulation);
		threadPoolSize = DEFAULT_THREAD_POOL_SIZE;
	}

	public void setThreadPoolSize(int threadPoolSize) {
		this.threadPoolSize = Math.max(threadPoolSize, 1);
	}

	@Override
	protected List<Specimen> nextGeneration(List<Specimen> population) {
		try {
			List<Specimen> newPopulation = Collections.synchronizedList(
					new ArrayList<Specimen>(populationSize));
			List<Thread> threadPool = new ArrayList<Thread>();
			for (int j = 0; j < threadPoolSize; j++) {
				int specimens = populationSize / threadPoolSize;
				if (j == 0) {
					specimens += populationSize % threadPoolSize;
				}
				Thread thread = new SpecimenCreatorThread(
						this, specimens, newPopulation, population);
				thread.start();
				threadPool.add(thread);
			}
			for (int j = 0; j < threadPool.size(); j++) {
				threadPool.get(j).join();
			}
			population = newPopulation;
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		return population;
	}
}
