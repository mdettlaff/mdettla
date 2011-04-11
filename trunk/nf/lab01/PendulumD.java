/*
 * Sterowanie odwróconym wahadlem
 * Przyklad z ksiażki T.J. Ross "Fuzzy Logic with Engineering Applications"
 *
 * Ruch wahadla opisuje uklad rownan roznicowych
 *
 * 		x1(t+1) = x1(t) + x2(t)
 *		x2(t+1) = x1(t) + x2(t) - u(t);
 *
 * gdzie x1(t) to kat wyhylenia wahadla, x2(t) to predkosc zmian
 * wyhylenia, natomiast u(t) to sterowanie (sila przykladana do wozka,
 * na ktorym znajduje sie wahadlo).
 *
 * Zmienna x1 przyjmuje jedna z trzech wartosci lingwistycznych:
 * NEG, Zero, POS. Odpowiadajace im trojkatne funkcje przynaleznosci
 * zdefiniowano w metodzie in1(). Podobne, lecz o innych nosnikach,
 * wartosci rozmyte przyjmuje zmienna x2 -- por. metoda in2().
 *
 * Do opisu sterowania wykorzystano 5 wartosci lingwistycznych.
 * Odpowiadajace im funkcje przynaleznosci definiuje metoda out().
 *
 * 9 regul opisujacych wybor sterowania dla konkretnych wartosci
 * lingwistycznych zapisano w postaci tablicy FAM (Fuzzy Associative
 * Memory): FAM[i][j] okresla wartosc lingwistyczna, ktora przyjmuje
 * sterowanie gdy 1-sza zmienna przyjela i-ta wartosc, a 2-ga zmienna
 * przyjela wartosc j-ta:
 *
 *        NEG | Zero | POS |
 * -------------------------
 * NEG  | NB  | N    | Z   |
 * -------------------------
 * Zero | N   | Z    | P   |
 * -------------------------
 * POS  | Z   | P    | PB  |
 */
class PendulumD {

	double INFTY = 1000;
	int[][] FAM = {{0,1,2}, {1,2,3}, {2,3,4}}; //baza regul

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
	 * Definiuje wejscie x_1
	 */
	double in1(double x, int what) {
		double y=INFTY;
		switch (what) {
			case 0: //{N}
				y=triangle(x,-INFTY,-2,0);
				break;
			case 1: //{Z}
				y=triangle(x,-2,0,2);
				break;
			case 2: //{P}
				y=triangle(x,0,2,INFTY);
				break;
			default: System.out.println("Bad value of WHAT "+what);
		}
		return y;
	}

	/*
	 * Definiuje wejscie x_2
	 */
	double in2(double x, int what) {
		double y=INFTY;
		switch (what) {
			case 0: //{N}
				y=triangle(x,-INFTY,-5,0);
				break;
			case 1: //{Z}
				y=triangle(x,-5,0,5);
				break;
			case 2: //{P}
				y=triangle(x,0,5,INFTY);
				break;
			default: System.out.println("Bad value of WHAT "+what);
		}
		return y;
	}

	/*
	 * Definiuje wyjscie
	 */
	double out(double x, int what) {
		double y=INFTY;
		switch (what) {
			case 0: //{NB}
				y=triangle(x,-24,-16,-8);
				break;
			case 1: //{N}
				y=triangle(x,-16,-8,0);
				break;
			case 2: //{Z}
				y=triangle(x,-8,0,8);
				break;
			case 3: //{P}
				y=triangle(x,0,8,16);
				break;
			case 4: //{PB}
				y=triangle(x,8,16,24);
				break;
			default: System.out.println("Bad value of WHAT "+what);
		}
		return y;
	}
	/*
	 * bada stopien zgodnosci obserwacji z przeslankami kolejnych regul
	 */
	void match(double x1, double x2) {
		double tau1, tau2;
		for (int i=0; i<tau.length; i++) tau[i]=0;
		for (int i=0; i<FAM.length;i++) {
			tau1 = in1(x1,i);
			for (int j=0; j<FAM[0].length; j++) {
				tau2 = in2(x2,j);
				tau[FAM[i][j]] = max(tau[FAM[i][j]], min(tau1, tau2));
			}
		}
	}

	/*
	 * wyostrzanie metoda srodka cieżkosci
	 */
	double defuzzify() {
		double h = 0.01; // krok calkowania
		double u = u_mn;
		double a = 0, b=0;
		double sum1=0, sum2=0;
		while (u<u_mx) {
			b=0;
			for (int i=0; i<u_ile; i++) {
				if (tau[i]>0) {
					a = min(out(u,i), tau[i]);
					b = max(a,b);
				}
			}
			if (b>0) {
				sum1 += u*b;
				sum2 += b;
			}
			u += h;
		}
		return (sum1/sum2);
	}

	void run(int max_iter) {
		double x1 = 1.0, x2 = -4.0;
		double u=0, tmp;

		for (int iter=0; iter<max_iter; iter++) {
			System.out.print("x1: "+x1+" x2: "+x2);
			match(x1,x2);
			u = defuzzify();
			System.out.println(" u: "+u);
			//model wahadla
			tmp = x1;
			x1 = x1 + x2;
			x2 = tmp+ x2 - u;
		}
	}

	public static void main(String[] args) {
		new PendulumD().run(4);
	}

	double min(double a,double b){
		if (a<b) return a;
		else return b;
	}

	double max(double a,double b){
		if (a<b) return b;
		else return a;
	}
	void myPause (int time) {
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {}
	}
}
