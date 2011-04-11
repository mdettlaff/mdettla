/*
 * Example 1 from Nozaki et al. (1997)
 */
class Example1 {

	double INFTY = 1000;
	int[][] FAM = {{0,1,2}, {1,2,3}, {2,3,4}}; //baza regul

	double[][] data = {{0.0, 0.641},
	                   {0.1, 0.698},
	                   {0.2, 0.678},
	                   {0.3, 0.591},
	                   {0.4, 0.469},
	                   {0.5, 0.359},
	                   {0.6, 0.302},
	                   {0.7, 0.322},
	                   {0.8, 0.409},
	                   {0.9, 0.531},
	                   {1.0, 0.641}};

	int x_ile = 6;//liczba rozmytych etykiet nadawanych wartosci wejsciowej
	double[] b = new double[x_ile]; //konkluzje generowanych regul

	int u_ile = 5;//liczba wartosci przyjmowanych przez sterowanie
	double u_mn = -24;
	double u_mx =  24;
	double[] tau = new double[u_ile];

	/*
	 * Trójkatna funkcja przynaleznosci
	 */
	double triangle(double x, double a, double b, double c) {
		if (x<=a|| x>= c)
			return 0.0;
		else
		if (a==-INFTY && x <= b)
			return 1.0;
		else
		if (c==INFTY && x >= b)
			return 1.0;
		else
		if (a <=x && x<=b)
			return (x-a)/(b-a);
		else
			return (c-x)/(c-b);
	}

	/*
	 * Definiuje wejscie x_1 (5 zbiorów)
	 */
	double in0(double x, int what) {
		double y=INFTY;
		switch (what) {
			case 0:
				y=triangle(x,-INFTY,0,0.25);
				break;
			case 1:
				y=triangle(x,0,0.25,0.50);
				break;
			case 2: //{P}
				y=triangle(x,0.25,0.50,0.75);
				break;
			case 3: //{P}
				y=triangle(x,0.50,0.75,1.0);
				break;
			case 4: //{P}
				y=triangle(x,0.75,1.0, INFTY);
				break;
			default: System.out.println("Bad value of WHAT "+what);
		}
		return y;
	}

	/*
	 * Definiuje wejscie x_1 (6 zbiorów)
	 */
	double in1(double x, int what) {
		double y=INFTY;
		switch (what) {
			case 0:
				y=triangle(x,-INFTY,0,0.20);
				break;
			case 1:
				y=triangle(x,0,0.20,0.40);
				break;
			case 2: //{P}
				y=triangle(x,0.20,0.40,0.60);
				break;
			case 3: //{P}
				y=triangle(x,0.40,0.6,0.8);
				break;
			case 4: //{P}
				y=triangle(x,0.60,0.8,1.0);
				break;
			case 5: //{P}
				y=triangle(x,0.80,1.0, INFTY);
				break;
			default: System.out.println("Bad value of WHAT "+what);
		}
		return y;
	}

	/*
	 * identyfikujemy reguly rozmyte zgodnie ze wzorami (9) i (10)
	 * [(4.45) i (4.46) w notatkach]
	 */
	void identify(double alpha) {
		double w,sum;
		for (int i=0; i<x_ile; i++) {
			sum = 0;
			for (int j=0; j<data.length; j++) {
				w = Math.pow(in1(data[j][0],i), alpha);
				sum = sum + w;
				b[i] = b[i] + w*data[j][1];
			}
			b[i] = b[i]/sum;
			System.out.println(i+" "+b[i]);
		}
		System.out.println("end of identification stage");
	}

	/*
	 * Generuje wyjscie w oparciu o zadana wartosć wejsciowa
	 * r-nanie (8) [(4.44) w notatkach]
	 */
	double infer(double x) {
		double s=0,y=0, z;
		for (int i=0; i<x_ile; i++) {
			z = in1(x, i);
			s += z;
			y += z*b[i]; // ważenie, konkluzja i-tej reguły
		}
		return (y/s);
	}

	double test_fun(double x) {
		return 0.2*Math.sin(2*Math.PI*x+Math.PI/4)+0.5;
	}

	void test() {
		double y,f, sum=0;
		double step = 0.01;
		double x = 0.0;
		System.out.println("Verification");
		while (x<=1.0) {
			y = infer(x);
			f = test_fun(x);
			sum += Math.pow(y-f,2);
			System.out.println(x+" "+y+" "+f);
			x += step;
		}
		System.out.println("err: "+sum);
	}

	void test1() {
		double y,f, sum=0;
		double step = 0.01;
		double x = 0.0;
		for (int i=0; i<data.length; i++) {
			y = infer(data[i][0]);
			f = test_fun(data[i][0]);
			sum += Math.pow(y-f,2);
			System.out.println(data[i][0]+" "+y+" "+f);
		}
		System.out.println("err: "+sum);
	}

	void run() {
		double x=0, h=0.1;
		int i = 0;
		while (x<=1.0) {
			data[i][0]=x; data[i][1]=test_fun(x);
			i++;
			x+=h;
		}
		identify(10);
		test();
	}

	public static void main(String[] args) {
		new Example1().run();
	}
}
