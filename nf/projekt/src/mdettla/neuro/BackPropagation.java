package mdettla.neuro;

import mdettla.util.Function;

public class BackPropagation {

	final int numInput; // liczba jednostek wejsciowych
	final int numHidden = 9; // liczba jednostek ukrytych

	final double eta = 0.039;
	final double targetErrorLevel = 0.05;

	double[][] weightsInputHidden;
//tego nie potrzebujesz - jutro mogę Ci wytłumaczyć
	double[][] deltaInputHidden;
	double[] weightsHiddenOutput = new double[numHidden + 1];
//tego nie potrzebujesz	
	double[] deltaHiddenOutput= new double[numHidden + 1];

	double outputErrorGradient;
	double[] hiddenErrorGradients = new double[numHidden];

	double[] HiddenNeurons = new double[numHidden];
	double output;
	double[] weightedSumInputHidden = new double[numHidden];
	double weightedSumHiddenOutput;

	private final double[][] dataIn;
	private final Function function;
	private final double[] target;

	public BackPropagation(double[][] dataIn, Function function) {
		numInput = dataIn[0].length;
		weightsInputHidden = new double[numInput + 1][numHidden];
		deltaInputHidden = new double[numInput + 1][numHidden];
		this.dataIn = dataIn;
		this.function = function;
		target = getDataOutputs();
		trainNetwork();
	}

	private double[] getDataOutputs() {
		double[] dataOut = new double[dataIn.length];
		for (int i = 0; i < dataIn.length; i++) {
			dataOut[i] = function.evaluate(dataIn[i]);
		}
		return dataOut;
	}

	public double getOutput(double[] xs) {
		for (int j = 0; j < numHidden; j++) {
			weightedSumInputHidden[j] = weightsInputHidden[numInput][j];
			for (int i = 0; i < numInput; i++) {
				weightedSumInputHidden[j] += xs[i] * weightsInputHidden[i][j];
			}
			HiddenNeurons[j] = firstActivation(weightedSumInputHidden[j]);
		}
		weightedSumHiddenOutput = weightsHiddenOutput[numHidden];
		for (int j = 0; j < numHidden; j++) {
			weightedSumHiddenOutput += HiddenNeurons[j] * weightsHiddenOutput[j];
		}
		output = secondActivation(weightedSumHiddenOutput);
		return output;
	}

	public double getError(double[][] xs) {
		double error = 0;
		for (double[] x : xs) {
			double expected = function.evaluate(x);
			double actual = getOutput(x);
			error += Math.pow(actual - expected, 2);
		}
		return error;
	}

	double secondActivation(double x) {
		return bipolarSigmoid(x);
	}

	double secondActivationDerivation(double x) {
		double tmp = secondActivation(x);
		return (1 - tmp*tmp)/2.;
	}

	double firstActivation(double x) {
		return bipolarSigmoid(x);
	}

	double secodActivationDerivation(double x) {
		double tmp = firstActivation(x);
		return (1 - tmp*tmp)/2.;
	}

	private double bipolarSigmoid(double x) {
		return 2.0/(1.0 + Math.exp(-x)) - 1;
	}

	void init() {
		double small = 0.8;
		for (int i=0; i < numInput + 1; i++) {
			for (int j = 0; j < numHidden; j++) {
				weightsInputHidden[i][j] = small * (2 * Math.random() - 1.);
			}
		}

		for (int j = 0; j < numHidden + 1; j++) {
			weightsHiddenOutput[j] = small * (2 * Math.random() - 1.);
		}
	}

	void trainNetwork() {
		boolean done=false;
		double err;

		init();
		int iter=0;
		while (!done) {
			err = 0;
			for (int x=0; x < dataIn.length; x++) {

				//feedforward
				for (int j=0; j<numHidden; j++) {
					weightedSumInputHidden[j] = weightsInputHidden[numInput][j];
					for (int i=0; i<numInput; i++) {
						weightedSumInputHidden[j] += dataIn[x][i]*weightsInputHidden[i][j];
					}
					HiddenNeurons[j] = firstActivation(weightedSumInputHidden[j]);
				}
				weightedSumHiddenOutput = weightsHiddenOutput[numHidden];
				for (int j=0; j<numHidden; j++) {
					weightedSumHiddenOutput += HiddenNeurons[j]*weightsHiddenOutput[j];
				}
				output = secondActivation(weightedSumHiddenOutput);
				
//gdyby to zrobić jak poniżej to będziesz miał wyjście LINIOWE, wytłumaczę Ci jutro po co
//możesz spróbować z nim wrzucając np.: 50k epok i mały learning rate
//				output = weightedSumHiddenOutput;


				//obliczenie bledu
				double a = target[x] - output;
				err += a*a;

				//Backpropagation
				//jednostka WY do jednostek ukrytych

				outputErrorGradient = a*secondActivationDerivation(weightedSumHiddenOutput);
//				deltaHiddenOutput[numHidden] = eta*outputErrorGradient;
				weightsHiddenOutput[numHidden] += eta*outputErrorGradient;
				for (int j=0; j<numHidden; j++) {
//					deltaHiddenOutput[j] = eta*outputErrorGradient*HiddenNeurons[j];
					weightsHiddenOutput[j] += eta*outputErrorGradient*HiddenNeurons[j];
				}

				//jednostki ukryte do jednostek WE
				for (int j=0; j<numHidden; j++) {
					hiddenErrorGradients[j] = outputErrorGradient*weightsHiddenOutput[j]*secodActivationDerivation(weightedSumInputHidden[j]);
//					deltaInputHidden[numInput][j] += eta*hiddenErrorGradients[j]; //eta*deltaY*w[j]*z[j]*(1-z[j]);
					weightsInputHidden[numInput][j] += eta*hiddenErrorGradients[j]; //eta*deltaY*w[j]*z[j]*(1-z[j]);
					for (int i=0; i<numInput; i++) {
//						deltaInputHidden[i][j] = eta*hiddenErrorGradients[j]*dataIn[x][i];
						weightsInputHidden[i][j] += eta*hiddenErrorGradients[j]*dataIn[x][i];
					}
				}

				//batch learning? gdyby tak to w innym miejscu ;)
				//synchroniczna aktualizacja wag
/*				for (int j=0; j<numHidden; j++) {
					for (int i=0; i<numInput+1; i++) {
						weightsInputHidden[i][j] += deltaInputHidden[i][j];
					}
					weightsHiddenOutput[j] += deltaHiddenOutput[j];
				}
				weightsHiddenOutput[numHidden] += deltaHiddenOutput[numHidden];
*/
			}
			if (err<targetErrorLevel || iter>10000) done=true;
			iter++;
		}
		System.out.println("done after " + iter + " iterations");
	}
}
