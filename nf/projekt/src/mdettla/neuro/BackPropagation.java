package mdettla.neuro;

import mdettla.util.Function;

class BackPropagation {

	int n = 2; //l-ba jednostek wejsciowych
	int h = 4; //l-ba jednostek ukrytych

	double[][] v = new double[n + 1][h];
	double[][] Dv = new double[n + 1][h];
	double[] w = new double[h + 1];
	double[] Dw= new double[h + 1];

	double deltaY;
	double[] deltaZ = new double[h];

	double[] z = new double[h];
	double y;
	double[] z_in = new double[h];
	double y_in;

	double eta = 0.19;
	double eps = 0.05;

	private final double[][] dataIn;
	private final Function function;
	private final double[] dataOut;

	public BackPropagation(double[][] dataIn, Function function) {
		this.dataIn = dataIn;
		this.function = function;
		dataOut = getDataOutputs();
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
		for (int j = 0; j < h; j++) {
			z_in[j] = v[n][j];
			for (int i = 0; i < n; i++) {
				z_in[j] += xs[i] * v[i][j];
			}
			z[j] = fZ(z_in[j]);
		}
		y_in = w[h];
		for (int j = 0; j < h; j++) {
			y_in += z[j] * w[j];
		}
		y = fY(y_in);
		return y;
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

	//bipolar sigmoid
	double fY(double x) {
		return 2.0/(1.0 + Math.exp(-x)) - 1;
	}

	double fYprim(double x) {
		double tmp = fY(x);
		return (1 - tmp*tmp)/2.;
	}

	//bipolar sigmoid
	double fZ(double x) {
		return 2.0/(1.0 + Math.exp(-x)) - 1;
	}

	double fZprim(double x) {
		double tmp = fZ(x);
		return (1 - tmp*tmp)/2.;
	}

	void init() {
		double small = 0.8;
		for (int i=0; i < n + 1; i++) {
			for (int j = 0; j < h; j++) {
				v[i][j] = small * (2 * Math.random() - 1.);
			}
		}

		for (int j = 0; j < h + 1; j++) {
			w[j] = small * (2 * Math.random() - 1.);
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
				for (int j=0; j<h; j++) {
					z_in[j] = v[n][j];
					for (int i=0; i<n; i++) {
						z_in[j] += dataIn[x][i]*v[i][j];
					}
					z[j] = fZ(z_in[j]);
				}
				y_in = w[h];
				for (int j=0; j<h; j++) {
					y_in += z[j]*w[j];
				}
				y = fY(y_in);


				//obliczenie bledu
				double a = dataOut[x] - y;
				err += a*a;

				//Backpropagation
				//jednostka WY do jednostek ukrytych

				deltaY = a*fYprim(y_in);
				Dw[h] = eta*deltaY;
				for (int j=0; j<h; j++) {
					Dw[j] = eta*deltaY*z[j];
				}

				//jednostki ukryte do jednostek WE
				for (int j=0; j<h; j++) {
					deltaZ[j] = deltaY*w[j]*fZprim(z_in[j]);
					Dv[n][j] += eta*deltaZ[j]; //eta*deltaY*w[j]*z[j]*(1-z[j]);
					for (int i=0; i<n; i++) {
						Dv[i][j] = eta*deltaZ[j]*dataIn[x][i];
					}
				}

				//synchroniczna aktualizacja wag
				for (int j=0; j<h; j++) {
					for (int i=0; i<n+1; i++) {
						v[i][j] += Dv[i][j];
					}
					w[j] += Dw[j];
				}
				w[h] += Dw[h];

			}
			if (err<eps || iter>1500) done=true;
			iter++;
		}
	}
}
