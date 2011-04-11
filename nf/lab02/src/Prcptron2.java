/*
 *Prymalny algorytm perceptronu.
 *Wiele wyjsc
 *Zamiast tablic mamy macierze (w porownaniu do poprzedniego algorytmu).
 */

class Prcptron2 {
	String f_name = "perc.txt";

	int m; 	//liczba obiektów
	int n;	//liczba wymiarów
	int k=3;//liczba klas

	double[][] x; //macierz obiektow

	double[][] Y; //wektor etykiet
	double[][] W; //wektor wagowy

	int[] perm;

	double[] B;
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
    	Y = new double[m][k];
    	W = new double[n][k];
    	B = new double[k];
    	perm = new int[m];

    	for (int i=0; i<m; i++) {
    		for (int j=0; j<n; j++) x[i][j] = x0[i][j];
    		for (int j=0; j<k; j++) Y[i][j] = -1;
    	}
    	for (int i=0; i<13; i++)  Y[i][0]=1;
    	for (int i=13; i<56; i++) Y[i][1]=1;
    	for (int i=56; i<m; i++)  Y[i][2]=1;

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
    	double[] tmp = new double[k];
    	int ile=0;
    	for (int i=0; i<m; i++) {
    		for (int j=0; j<k; j++) {
    			tmp[j] = B[j];
    			for (int l=0; l<n; l++) {
    				tmp[j] += W[l][j]*x[perm[i]][l];
    			}
    			if (Y[perm[i]][j]*tmp[j] <= 0) {
    				for (int l=0; l<n; l++) {
    					W[l][j] += eta*Y[perm[i]][j]*x[perm[i]][l];
    				}
    				B[j] += eta*Y[perm[i]][j]*R;
    				ile++;
    			}
    		}
    	}
    	return ile;
    }

	void run() {
		initTables();
		findR();
		System.out.println("Perceptron z wieloma wyjsciami "+f_name);
		System.out.println("m: "+m+" n: "+n);
		int changes=0;
		int iter=0;
		for (int i=0; i<m;i++) perm[i]=i;
		randPerm();
		do {
			changes=singleEpoch();
			System.out.println("   #changes: "+changes);
			iter++;
		} while (changes>0);
		System.out.println("iter: "+iter);
		for (int i=0; i<n; i++) {
			for (int j=0; j<k; j++) {
				System.out.print(W[i][j]+" ");
			}
			System.out.println();
		}
		for (int j=0; j<k; j++) {System.out.print(B[j]+" ");}
		System.out.println();
		testuj();
	}
	/*
	 *Sprawdzam poprawnoć klasyfikacji
	 */
	void testuj() {
		double tmp;
		System.out.println();
		System.out.println("Klasyfikacja: ");
    	for (int i=0; i<m; i++) {
    		for (int j=0; j<k; j++) {
    			tmp = B[j];
    			for (int l=0; l<n; l++) {
    				tmp += W[l][j]*x[i][l];
    			}
    			if (tmp<0) System.out.print("-1 ");
    				else System.out.print("1 ");
    		}
    		System.out.println();
		}
	}

	public static void main(String[] s) {
		new Prcptron2().run();
	}

  void pressAKey() {
	System.out.println("Press ENTER");
	try {
      	System.in.read();
	} catch(Exception e) {System.out.println("cos");};
  }
}
