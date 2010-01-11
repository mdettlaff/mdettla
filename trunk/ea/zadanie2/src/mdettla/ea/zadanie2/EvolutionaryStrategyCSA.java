package mdettla.ea.zadanie2;

/**
 * Cumulative step adaptation.
 * dobra wersja
 * http://www.informatik.uni-ulm.de/ni/Lehre/SS02/Evosem/es.java
 */
public class EvolutionaryStrategyCSA {
	/** number of generations */
	private final int generationsCount;
	/** population size */
	private final int mu;
	/** temporary population size */
	private final int lambda = 100;

	/** minimum value for standard deviations */
//	private final double epsilon = 0.05;

	/** initial value of standard deviations */
//	private final double stddevdef = 3.0;
	/** number of decision variables */
	private final int varsCount;

	private FitnessFunction fitness;

	public EvolutionaryStrategyCSA(FitnessFunction fitness,
			int varsCount, int generationsCount, int populationSize) {
		this.fitness = fitness;
		this.varsCount = varsCount;
		this.generationsCount = generationsCount;
		mu = populationSize;
	}

	/** calculate the fitness of an Individuum */
//	public static double fitness(Individuum i) {
//		double fitness = 0.0;
        // Schwefel's Sine Root Function - defined for -500 < x_i < 500
        // minimum at x_i = 420.9687
		/*        fitness = 418.9829 * varsCount;
        for (int j = 0; j < varsCount; j++) {
            if (Math.abs(i.vars[j]) > 500) return Double.MAX_VALUE; // penalty
            fitness -= i.vars[j]*Math.sin(Math.sqrt(Math.abs(i.vars[j]))); 
        }
        // Griewangk's Function - defined for -600.0 < x_i < 600.0
        // minimum at x_i = 0 - this thing is _very_ multimodal
		  *//*        double sum = 0.0, prod = 1.0;
        for (int j = 0; j < varsCount; j++) {
            sum += i.vars[j] * i.vars[j] / 4000.0;
            prod *= Math.cos((i.vars[j]) / Math.sqrt(j + 1));
        }
        fitness = 1.0 + sum - prod;
        // Rastrigin's Function - defined for -5.12 <= x_i < 5.12
        // minimum at x_i = 0
		   *//*        fitness = 10.0 * varsCount;
        for (int j = 0; j < varsCount; j++) {
            if (Math.abs(i.vars[j]) > 5.12) return Double.MAX_VALUE; // penalty
            fitness += i.vars[j]*i.vars[j]-10.0*Math.cos(2.0*Math.PI*i.vars[j]);
        }
		*/
//		return fitness;
//	}

	// stuff below this line should not be changed (unless it has bugs......) ----

	public void runAlgorithm() {
		/** our source of random numbers */
		java.util.Random rnd = new java.util.Random();
		/** the population */
		Individuum[] pop = new Individuum[mu];
		/** the best Individuum found so far */
		Individuum best = new Individuum(varsCount);
		best.fitness = Double.MAX_VALUE;
		/** in which generation the best Individuum has been found */
//		int bestg = 0;

		double sigma = 3.0;
		double chi_n = Math.sqrt(varsCount)*(1 - 1.0/(4.0*varsCount) + 1.0/(21.0*varsCount*varsCount));
		double dump = Math.sqrt(varsCount);
		double c    = 1/dump;
		double mult1 = 1-c;
		double mult2 = Math.sqrt(c*(2-c)*mu);

		double[] tt = new double[varsCount];

		for (int j = 0; j < varsCount; j++) {
			tt[j] = (rnd.nextDouble() - 0.5) * 2.0 * fitness.getArea();
		}
		// generate the intial population and calculate fitness
		for (int i = 0; i < mu; i++) {
			pop[i] = new Individuum(varsCount);
			for (int j = 0; j < varsCount; j++) {
				pop[i].vars[j] = tt[j] + sigma*rnd.nextGaussian();
			}
			pop[i].fitness = fitness.fitness(pop[i]);
			best = (best.compareTo(pop[i]) < 0) ? best : pop[i];
		}
		System.out.println("Best in initial population:\n" + best);

		double[] x_old = new double[varsCount];
		double[] x_new = new double[varsCount];
		double[] path  = new double[varsCount];

		//implementacja r-nania (6.63)
		for (int j = 0; j < varsCount; j++) {
			x_old[j] = 0;
			for (int k=0; k<mu; k++) {
				x_old[j] += pop[k].vars[j];
			}
			x_old[j] = x_old[j]/mu;
		} 

		// the generation loop
		for (int i = 1; i <= generationsCount; i++) {
			/** the temporary population */
			Individuum[] tpop = new Individuum[lambda+mu];

			for (int j = 0; j < lambda; j++) {

				Individuum child = new Individuum(varsCount);

				for (int k=0; k<varsCount; k++) {
					child.vars[k] = x_old[k] + sigma*rnd.nextGaussian();
				}
				// evaluate the child
				child.fitness = fitness.fitness(child);
				tpop[j] = child;
			}

			for (int j=0; j<mu; j++) {
				tpop[lambda+j]=pop[j];
			}

			java.util.Arrays.sort(tpop); // sorts ascending

			for (int j = 0; j < mu; j++) {
				pop[j] = tpop[j];
				//System.out.println(pop[j]);
			}

			//wyznaczenie nowej sredniej
			for (int j = 0; j < varsCount; j++) {
				x_new[j] = 0;
				for (int k=0; k<mu; k++) {
					x_new[j] += pop[k].vars[j];
				}
				x_new[j] = x_new[j]/mu;
			}

			//modyfikacja sciezki
			double d = 0;
//			double delta;
			for (int j = 0; j < varsCount; j++) {
				path[j] = path[j]*mult1;
				path[j]+= mult2/Math.sqrt(sigma)*(x_new[j]-x_old[j]);
//				delta = x_new[j]-x_old[j];
				d = d + path[j]*path[j];
				x_old[j] = x_new[j];
			}

			d = Math.sqrt(d);
			d = (d-chi_n)/dump/chi_n;
			//nowe sigma
			sigma = sigma*Math.exp(d);
			if (sigma<0.0001) sigma = 0.0001;
			else
				if (sigma>100) sigma = 100;
			if (best.compareTo(pop[0]) > 0) {
				best = pop[0];
//				bestg = i;
			}
			System.out.println("Generation: " + i + " Best Individuum: " + pop[0] +
					String.format(" sigma: %.5f", sigma));
		}
		//System.out.println("Overall best Individuum in generation: " + bestg);
		//System.out.println(best);
	}

//	public static void pressAKey() {
//		System.out.println("Press ENTER");
//		try {
//			System.in.read();
//		} catch(Exception e) {}
//	}	    
}
