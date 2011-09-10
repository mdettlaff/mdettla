package mdettla.keyboard.ga;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import mdettla.jga.core.ConcurrentGeneticAlgorithm;
import mdettla.jga.core.CrossoverOperator;
import mdettla.jga.core.GeneticAlgorithm;
import mdettla.jga.core.MutationOperator;
import mdettla.jga.core.SelectionFunction;
import mdettla.jga.core.Specimen;
import mdettla.jga.operators.crossover.CycleCrossover;
import mdettla.jga.operators.mutation.SwapMutation;
import mdettla.jga.operators.selection.AbstractTournamentSelection;
import mdettla.jga.operators.selection.MultiobjectiveMajorityTournamentSelection;

public class GAKeyboard {

	private int populationSize;
	private int generationsCount;
	private int eliteSize;
	private int tournamentSize;
	private double mutationProbability;
	private MutationOperator mutationOperator;
	private double crossoverProbability;
	private CrossoverOperator crossoverOperator;
	private SelectionFunction selectionFunction;
	private List<File> textFiles;
	private boolean quiet;
	private int epochsCount;

	public GAKeyboard() {
		populationSize = 50;
		generationsCount = 400;
		eliteSize = 2;
		tournamentSize = 4;
		mutationProbability = .7;
		mutationOperator = new SwapMutation();
		crossoverProbability = .7;
		crossoverOperator = new CycleCrossover();
		selectionFunction = new MultiobjectiveMajorityTournamentSelection(tournamentSize);
		textFiles = new ArrayList<File>();
		quiet = false;
	}

	public static void main(String[] args) throws Exception {
		GAKeyboard algorithm = new GAKeyboard();
		algorithm.initializeFromArgs(args);
		if (algorithm.epochsCount == 0) {
			algorithm.run();
		} else {
			algorithm.runManyEpochsAverage();
		}
	}

	private void initializeFromArgs(String[] args)
			throws InstantiationException, IllegalAccessException,
			ClassNotFoundException, FileNotFoundException {
		GetOpt getOpt = new GetOpt(args);
		if (getOpt.isOptionSet("populationSize")) {
			populationSize = getOpt.getIntValue("populationSize");
		}
		if (getOpt.isOptionSet("generationsCount")) {
			generationsCount = getOpt.getIntValue("generationsCount");
		}
		if (getOpt.isOptionSet("eliteSize")) {
			eliteSize = getOpt.getIntValue("eliteSize");
		}
		if (getOpt.isOptionSet("tournamentSize")) {
			tournamentSize = getOpt.getIntValue("tournamentSize");
		}
		if (getOpt.isOptionSet("mutationProbability")) {
			mutationProbability = getOpt.getDoubleValue("mutationProbability");
		}
		if (getOpt.isOptionSet("mutationOperator")) {
			mutationOperator = (MutationOperator) Class.forName(
					getOpt.getValue("mutationOperator")).newInstance();
		}
		if (getOpt.isOptionSet("crossoverProbability")) {
			crossoverProbability = getOpt.getDoubleValue("crossoverProbability");
		}
		if (getOpt.isOptionSet("crossoverOperator")) {
			crossoverOperator = (CrossoverOperator) Class.forName(
					getOpt.getValue("crossoverOperator")).newInstance();
		}
		if (getOpt.isOptionSet("selectionFunction")) {
			selectionFunction = (SelectionFunction) Class.forName(
					getOpt.getValue("selectionFunction")).newInstance();
			if (selectionFunction instanceof AbstractTournamentSelection) {
				((AbstractTournamentSelection)selectionFunction).setTournamentSize(tournamentSize);
			}
		}
		for (String arg : getOpt.getArguments()) {
			File file = new File(arg);
			if (!file.exists()) {
				throw new FileNotFoundException(arg);
			}
			textFiles.add(file);
		}
		if (getOpt.isOptionSet("quiet")) {
			quiet = getOpt.getBooleanValue("quiet");
		}
		if (getOpt.isOptionSet("epochsCount")) {
			epochsCount = getOpt.getIntValue("epochsCount");
		}
	}

	public void run() throws IOException {
		TextStatistics stats = getTextStatistics(textFiles);
		List<Specimen> initialPopulation = getInitialPopulation(stats);

		GeneticAlgorithm ga = new ConcurrentGeneticAlgorithm(initialPopulation);
		initializeFromParameters(ga);

		Specimen best = ga.runEpoch(generationsCount);

		Specimen random = KeyboardLayout.createRandomInstance(stats);
		random.computeFitness();
		print("\nLosowy układ:\n" + random);
		print("\nQWERTY:\n" + KeyboardLayout.getQWERTYLayout(stats));
		print("\nDvorak:\n" + KeyboardLayout.getDvorakLayout(stats));
		print("\nCall Me E-Mail:\n" + KeyboardLayout.getCallMeEmailLayout(stats));
		print("\nSimulated Annealing:\n" + KeyboardLayout.getSimulatedAnnealingLayout(stats));
		System.out.println("\nNajlepiej przystosowany osobnik:\n" + best);
		compareObjectives((KeyboardLayout) best, KeyboardLayout.getDvorakLayout(stats));
	}

	private void initializeFromParameters(GeneticAlgorithm ga) {
		ga.setMutationOperator(mutationOperator);
		ga.setMutationProbability(mutationProbability);
		ga.setCrossoverOperator(crossoverOperator);
		ga.setCrossoverProbability(crossoverProbability);
		ga.setSelectionFunction(selectionFunction);
		ga.setEliteSize(eliteSize);
		ga.setQuiet(quiet);
		System.out.println(getInitialParameters(ga) + "\n");
	}

	private void runManyEpochsAverage() throws IOException {
		TextStatistics stats = getTextStatistics(textFiles);
		List<Specimen> initialPopulation = getInitialPopulation(stats);
		GeneticAlgorithm ga = new ConcurrentGeneticAlgorithm(initialPopulation);
		initializeFromParameters(ga);
		System.out.println("epochsCount = " + epochsCount + "\n");
		List<List<Double>> results = new ArrayList<List<Double>>();
		ga.computeFitness(initialPopulation);
		for (int epoch = 0; epoch < epochsCount; epoch++) {
			System.err.println("epoch " + (epoch + 1));
			int iteration = 0;
			List<Specimen> population = new ArrayList<Specimen>(initialPopulation);
			for (int i = 0; i < generationsCount; i++) {
				population = ga.nextGeneration(population);
				ga.computeFitness(population);
				Specimen best = Collections.max(population);
				if (iteration >= results.size()) {
					results.add(new ArrayList<Double>());
				}
				results.get(iteration).add(best.getFitness().doubleValue());
				iteration++;
				if (iteration == generationsCount) {
					break;
				}
			}
		}
		for (int iter = 0; iter < results.size(); iter++) {
			double sum = 0;
			for (int j = 0; j < results.get(iter).size(); j++) {
				sum += results.get(iter).get(j);
			}
			double average = sum / epochsCount;
			double squareDiffsSum = 0;
			for (int j = 0; j < results.get(iter).size(); j++) {
				squareDiffsSum += Math.pow(results.get(iter).get(j) - average, 2);
			}
			double stdDev = Math.sqrt(squareDiffsSum / epochsCount);
			System.out.println("average: " + (iter + 1) + " " + average);
			System.out.println("std dev: " + (iter + 1) + " " + stdDev);
		}
	}

	private TextStatistics getTextStatistics(List<File> corpus) throws IOException {
		if (corpus.isEmpty()) {
			corpus = Arrays.asList(new File(
					"src/mdettla/keyboard/ga/resources/en/pg4300.txt"));
		}
		TextStatistics stats = new TextStatistics();
		for (File textFile : corpus) {
			stats.read(new InputStreamReader(new FileInputStream(textFile)));
		}
		print("długość analizowanego tekstu: " + stats.getTextLength() + " znaków");
		return stats;
	}

	private List<Specimen> getInitialPopulation(TextStatistics stats) {
		List<Specimen> initialPopulation = new ArrayList<Specimen>();
		for (int i = 0; i < populationSize; i++) {
			initialPopulation.add(KeyboardLayout.createRandomInstance(stats));
		}
		return initialPopulation;
	}

	private String getInitialParameters(GeneticAlgorithm ga) {
		StringBuilder sb = new StringBuilder();
		sb.append("rozmiar populacji: " + populationSize + "\n");
		sb.append("ilość iteracji: " + generationsCount + "\n");
		sb.append("rozmiar elity: " + ga.getEliteSize() + "\n");
		sb.append("rozmiar turnieju: " + tournamentSize + "\n");
		sb.append("prawdopodobieństwo mutacji: " + ga.getMutationProbability() + "\n");
		sb.append("prawdopodobieństwo krzyżowania: " + ga.getCrossoverProbability() + "\n");
		sb.append("operator mutacji: " +
				ga.getMutationOperator().getClass().getSimpleName() + "\n");
		sb.append("operator krzyżowania: " +
				ga.getCrossoverOperator().getClass().getSimpleName() + "\n");
		sb.append("operator selekcji: " +
				ga.getSelectionFunction().getClass().getSimpleName());
		return sb.toString();
	}

	private void compareObjectives(KeyboardLayout layout, KeyboardLayout other) {
		for (int i = 0; i < layout.OBJECTIVES.length; i++) {
			print(String.format("%s: %s (%.3f%%)", layout.OBJECTIVES[i].getName(),
					layout.OBJECTIVES[i].getValue() < other.OBJECTIVES[i].getValue(),
					(1 - layout.OBJECTIVES[i].getValue() / other.OBJECTIVES[i].getValue()) * 100));
		}
	}

	private void print(String s) {
		if (!quiet) {
			System.out.println(s);
		}
	}
}
