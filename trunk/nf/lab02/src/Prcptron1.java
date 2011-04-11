/*
 *Prymalny algorytm perceptronu.
 *
 */

class Prcptron1 {
//	String f_n="d:\\Java\\Nauka\\Fuzzy\\FClustering\\p01\\Data\\";
	String f_name = "perc.txt"; //f_n+"data3_2.txt"; //"iris.txt"; //"iris.txt";

	int m; 	//liczba obiektów
	int n;	//liczba wymiarów

	double[][] x; //macierz obiektow

	double[] y; //wektor etykiet
	double[] w; //wektor wagowy

	int[] perm;

	double b;
	double R; //parametr R^2

	double eta=0.9;

	ReadData rd = new ReadData(f_name);
	java.util.Random rnd = new java.util.Random();

    void initTables() {
		double[][] x0; //macierz obiektow
    	m = rd.getCard();
    	n = rd.getDim();
    	x0= new double[m][n];
    	x0= rd.getX();
    	n=n-1;
    	x = new double[m][n];
    	y = new double[m];
    	w = new double[m];

    	perm = new int[m];

    	for (int i=0; i<m; i++) {
    		for (int j=0; j<n; j++) x[i][j] = x0[i][j];
    		y[i] = x0[i][n];
    	}
    }

	/* losowa permutacja */
	void randPerm(){
		int i,rp,sw; //loop index, random position, swap
		for(i=0;i<m;i++)perm[i]=i; //fill in the identity permutation
		for(i=0;i<m-1;i++){//for all entries
			rp=(int)((m-i)*Math.random()+i); //get a random number in the range i..(n-1)
			sw=perm[i]; //swap the
			perm[i]=perm[rp]; //current
			perm[rp]=sw; //and random entry
		}
	}

    /*
     *oblicza kwadrat normy wektora x
     */
    double sNorm(double[] x) {
    	double s=0;
    	for (int j=0; j<n; j++) {
    		s += x[j]*x[j];
    	}
    	return s;
    }

    void findR() {
    	R = 0;
    	double tmp;
    	for (int i=0; i<m; i++) {
    		tmp = sNorm(x[i]);
    		if (tmp > R) {
    			R = tmp;
    		}
    	}
    }

    int singleEpoch() {
    	double tmp;
    	int ile=0;
    	for (int i=0; i<m; i++) {
    		tmp = b;
    		for (int j=0; j<n; j++) {
    			tmp += w[j]*x[perm[i]][j];
    		}
    		if (y[perm[i]]*tmp <= 0) {
    			for (int j=0; j<n; j++) {
    				w[j] += eta*y[perm[i]]*x[perm[i]][j];
    			}
    			b += eta*y[perm[i]]*R;
    			ile++;
    		}
    	}
    	return ile;
    }

	void run() {
		System.out.println("PCA "+f_name);
//		System.out.println("testuj");
		initTables();
		findR();
		System.out.println("m: "+m+" n: "+n);
		int changes=0;
		int iter=0;
		for (int i=0; i<m;i++) perm[i]=i;
		randPerm();
		do {
			changes=singleEpoch();
			//System.out.println("ch: "+changes);
			iter++;
		} while (changes>0);
		System.out.println("iter: "+iter);
		for (int i=0; i<n; i++) {
			System.out.print("w[" + i + "] = " + w[i] + " ");
		}
		System.out.println("\nb = " + b);
		testuj();
	}

	void testuj() {
		System.out.println("testuj");
		randPerm();
		double teta;
		for (int i=0; i<m; i++) {
			teta = b;
			for (int j=0; j<n; j++) {
				teta += w[j]*x[perm[i]][j];
			}
			if (teta<0)
				System.out.println(i+" expected: "+y[perm[i]]+", actual: -1");
			else
				System.out.println(i+" expected: "+y[perm[i]]+", actual: 1");
		}
	}

	public static void main(String[] s) {
		new Prcptron1().run();
	}

  void pressAKey() {
	System.out.println("Press ENTER");
	try {
      	System.in.read();
	} catch(Exception e) {System.out.println("cos");};
  }

}
