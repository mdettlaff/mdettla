package mdettla.neuro;

import mdettla.util.Function;

public class BackPropagation {

	final int numInput; // ilość jednostek wejściowych
	final int numHidden = 9; // ilość jednostek ukrytych

	final double eta = 0.039;
	final double targetErrorLevel = 0.05;

	double[][] weightsInputHidden;
	double[] weightsHiddenOutput = new double[numHidden + 1];

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
			for (int x = 0; x < dataIn.length; x++) {

				// propagacja wprzód
				for (int j = 0; j < numHidden; j++) {
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

				// output = weightedSumHiddenOutput;

				double a = target[x] - output;
				err += a*a;

				// propagacja wstecz

				outputErrorGradient = a*secondActivationDerivation(weightedSumHiddenOutput);
				weightsHiddenOutput[numHidden] += eta*outputErrorGradient;
				for (int j=0; j<numHidden; j++) {
					//					deltaHiddenOutput[j] = eta*outputErrorGradient*HiddenNeurons[j];
					weightsHiddenOutput[j] += eta*outputErrorGradient*HiddenNeurons[j];
				}

				//jednostki ukryte do jednostek WE
				for (int j=0; j<numHidden; j++) {
					hiddenErrorGradients[j] =
						outputErrorGradient*weightsHiddenOutput[j]*secodActivationDerivation(weightedSumInputHidden[j]);
					weightsInputHidden[numInput][j] += eta*hiddenErrorGradients[j];
					for (int i=0; i<numInput; i++) {
						weightsInputHidden[i][j] += eta*hiddenErrorGradients[j]*dataIn[x][i];
					}
				}
			}
			if (err < targetErrorLevel || iter > 10000) {
				done = true;
			}
			iter++;
		}
		System.out.println("done after " + iter + " iterations");
	}
}
