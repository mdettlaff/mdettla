/*
 * PROBLEM:
 *
 * 10 kart ponumerowanych od 1 do 10 podzielic na dwa zbiory
 * w taki sposób, aby: 
 * (a) suma numerów kart zaliczonych do 1-go zbioru byla bliska SUMTARG=36
 * (b) iloczyn numerow kart z 2go zbioru byl bliski PRODTARG=360
 *
 * W programie zastosowano: 
 * (i)   selekcje turniejowa (o rozmiarze 2)
 * (ii)  krzyzowanie 1-punktowe z pstwem P_REC
 * (iii) mutacje probabilistyczna z pstwem P_MUT
 *
 * Populacja sklada sie z SIZE=30 osobnikow, 
 * a kazdy osobnik z LEN=10 genow 
 * Budowa chromosomu: 
 *      0 - karta nalezy do 1-go zbioru, 
 *      1 - karta nale¿y do 2-go zbioru
 *
 * Pojedynczy przebieg (epoka) AG obejmuje MAX_ITER=1000 iteracji.
 * Wyniki stanowia usrednienie po MAX_EPOCH=50 epokach.
 * Dla kazdej epoki podane jest najlepsze znalezione rozwiazanie
 * oraz iteracja, w ktorej to nastapilo.
 * Potem drukowane sa: nr iteracji, wartosc srednia najlepszego
 * rozwiazania.
 *
 * Uruchamianie:
 * java Cards > wynik.txt
 */
 
import java.util.Random;

class Cards {
	
    Random rnd = new Random();
    
    static int SIZE = 30; // rozmiar populacji
    static int LEN = 10;  // dlugosc chromosomu
    
    double SUMTARG  = 36; // suma kart z 1-go zbioru
    double PRODTARG = 360;// iloczyn kart z 2-go zbioru 
    
    int MAX_ITER = 1000;  // max. lba iteracji
    int MAX_EPOCH =  50;  // lba epok
    
	int[][] oldpop = new int[SIZE][LEN]; 
	int[][] newpop = new int[SIZE][LEN];
	
	double[] fit  = new double[SIZE];		   // dostosowanie osobników 
	double[][] stat = new double[MAX_ITER][2]; // statystyki
	int[] cbest = new int[LEN];				   // najlepszy osobnik
	double best = Double.MAX_VALUE;			   // najlepsza wartosc dostosowania
	boolean pisac;
	
	double totFit; 							   // calkowite dopasowanie
	
    /** Zwraca dopasowanie genu, tzn. wartosc odstepstwa od 
     *  zadanych wartosci SUMTARGD i PRODTARG;
     *  Jezeli i-ta pozycja genu = 0 wskazuje ona na przynaleznosc 
     *  karty do pierwszego zbioru, a 1 - do drugiego zbioru.
     *  Jezeli sum = suma elementow, a prod - ich iloczyn, to blad 
     *  dotyczacy zaproponowanego rozdzialu jest rowny 
     *  |sum - SUMTARG|/SUMTARG + |prod - PRODTARG|/PRODTARG
     */
    double fitness(int gene) {
    	int sum = 0;
    	int prod= 1;
    	for (int j=0; j<LEN; j++) {
    		if (oldpop[gene][j]==0) {
    			sum += (j+1);
    		} else {
    			prod *= (j+1);
    		}
    	}
    	return Math.abs(sum - SUMTARG)/SUMTARG 
    	       + Math.abs(prod - PRODTARG)/PRODTARG;
    }
    
    /** inicjalizacja populacji 
     *  oraz najlepszego aktualnego rozwiazania*/
    void init() {
    	for (int i=0; i<SIZE; i++) {
    		for (int j=0; j<LEN; j++) {
    			if (rnd.nextDouble() < 0.5)
    				oldpop[i][j] = 0;
    			else
    				oldpop[i][j] = 1;
    		}
    	}
    	for (int j=0; j<LEN; j++) {
    		cbest[j] = oldpop[0][j];
    	}
		best = fitness(0);
    }
    
    /** turniej o rozmiarze 2 
     *  zwraca numer lepszego osobnika,
     *  (tzn. osobnika o nizszej wartosci fit)
     *   */
    int tournament() {
    	int ch1,ch2;
		ch1 = (int)Math.floor(Math.random()*SIZE);
		do {
			ch2 = (int)Math.floor(Math.random()*SIZE);
		} while (ch1==ch2);
		if (fit[ch1]<fit[ch2]) return ch1;
		else return ch2;
    }
    
    /** pojedyncze wywowlanie algorytmu, tzn.
     *  pojedyncza epoka
     */
    void one_epoch() {
    
    	double P_MUT = 0.1; // p-stwo mutacji
    	double P_REC = 0.6; // p-stwo krzyzowania
    
		int poz=0;			// pozycja najlepszego osobnika
		
		int ch1,ch2, cross, imin=0, imax=0;
		double min=Double.MAX_VALUE, max;
		
		init();
	
		// ewolucja przez MAX_ITER iteracji
		for (int iter = 0; iter < MAX_ITER; iter++) {
			//ocena osobnikow
			min = Double.MAX_VALUE;
			max = 0;
			totFit = 0; 
			double suma=0;
			for (int i=0; i<SIZE; i++) {
				fit[i] = fitness(i);     // oblicza dostosowanie i-tego osobnika
				totFit+=fit[i]; 		 // obl. calkowitego dopasowania 
				if (fit[i]<min) {
					min = fit[i];  		 // najlepsza wartosc dostosowania
					imin = i;	   		 // nr pozycji najlepszego osobnika
				}
				if (fit[i]>max) {
					max = fit[i];
					imax = i;
				}
			}
			if (iter==999) piszPop();
			//strategia elitarna:
			if (min>best) {
			//jesli nowa populacja nie zawiera lepszego
			//osobnika, to najgorszy zastepowany jest przez cbest
				for (int j=0; j<LEN; j++)
				oldpop[imax][j] = cbest[j];
				min = best;
				fit[imax] = best;
				suma = totFit - fit[imax] + min;
			} 
			//w przeciwnym razie najlepszy z populacji staje sie cbest;
			else {
				for (int j=0; j<LEN; j++) {
					cbest[j] = oldpop[imin][j];
				}
				best = min;
			}
			suma = suma/SIZE;
			//druk najlepszego rozwiazania	
	    	if (min==0 && pisac) {
				System.out.print("Rozwiazanie: '");
				for (int i=0; i<LEN; i++) System.out.print(oldpop[imin][i]);
				System.out.println(" fit: "+fitness(imin)+" iteracja: "+iter);
				pisac=false;
			}
			// zbieranie statystyk
	    	stat[iter][0] += min;
	    	stat[iter][1] += min*min;
	    	poz = 0;
	    	//operacje genetyczne: selekcja, krzyzowanie i mutacja
	    	for (int j = 0; j < SIZE/2; j++) {
				// Wybór rodziców (selekcja turniejowa).
				ch1 = tournament();
				ch2 = tournament();
				//krzyzowanie
				if (rnd.nextDouble()<P_REC) {
					cross = (int)Math.floor(rnd.nextDouble()*LEN);
				} else cross=LEN; //bez krzyzowania
				for (int i=0; i<cross; i++) {
					//mutacja
					if (rnd.nextDouble()<P_MUT) 
						newpop[poz][i] = 1-oldpop[ch1][i];
					else 
						newpop[poz][i] = oldpop[ch1][i];
					if (rnd.nextDouble()<P_MUT) 
						newpop[poz+1][i] = 1-oldpop[ch2][i];
					else 
						newpop[poz+1][i] = oldpop[ch2][i];
				}
				for (int i=cross; i<LEN; i++) {
					if (rnd.nextDouble()<P_MUT)
						newpop[poz][i] = 1-oldpop[ch2][i];
					else 
						newpop[poz][i] = oldpop[ch2][i];
					if (rnd.nextDouble()<P_MUT) 
						newpop[poz+1][i] = 1-oldpop[ch1][i];
					else 
						newpop[poz+1][i] = oldpop[ch1][i];
				}
				poz = poz+2;
	    	}
	    	for (int i=0; i<SIZE; i++) {
	    		for (int j=0; j<LEN; j++) {
	    			oldpop[i][j] = newpop[i][j];
	    		}
	    	}	    	
		}
    }
    
    void run() {
    	for (int e=0; e<MAX_EPOCH; e++) {
    		pisac=true;
    		one_epoch();
    	}
    	int tt = MAX_EPOCH*SIZE;
    	for (int i=0; i<MAX_ITER; i++) {
    		stat[i][0] = stat[i][0]/MAX_EPOCH;
    		stat[i][1] = (stat[i][1] - MAX_EPOCH*stat[i][0]*stat[i][0]);
    		stat[i][1] = Math.sqrt(stat[i][1]/(MAX_EPOCH-1));
    	}
    	for (int i=0; i<MAX_ITER; i++) {
    		System.out.println(i+" "+stat[i][0]+" "+stat[i][1]); //+" "+stat[i][2]+" "+stat[i][3]));
    	}
    }

    public static void main(String[] args) {
    	new Cards().run();
    }

	void piszPop() {
		for (int i=0; i<SIZE; i++) {
			for (int j=0; j<LEN; j++) {
				System.out.print(oldpop[i][j]);
			}
			System.out.println(" "+fit[i]);
		}
		System.out.println();
	}

    void pressAKey() {
		System.out.println("Press ENTER");
		try {
      		System.in.read();
		} catch(Exception e) {}
	}
	
	
}
