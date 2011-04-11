/*
 *Program do czytania danych z pliku tekstowego;
 *
 *Fromat pliku:
 *
 *pierwsza linia zawiera trzy liczby:
 *  m - ilosc obserwacji
 *  n - ich wymiar
 *  k - liczba klas
 *
 *w kolejnych liniach zapisane sa pojedyncze punkty
 *
 *Program czyta tylko kolejne wymiary i interesuje się, czy coś
 *jest jeszcze w danej linii.
 *
 *Metody getCard(), getDim(), getClust() zwracaja dane z pierszej linii,
 *metoda getX() zwraca macierz obserwacji wymiaru m x n.
 */
//package Myutils;

import java.util.*;
import java.io.*;

class ReadData {

	int m; //liczba punktów
	int n; //ich wymiar
	int k; //liczba klas

	double[][] x; //dane

	/*
	 *Konstruktor
	 */
	ReadData(String s) {
		readData(s);
	}

	/*
	 *czytanie informacji typu integer z pojedynczej linii tekstu
	 **/
	int[] readIntField(String text, int j) {
            int value1, x=0;
            boolean done=false;
     		char ch;
        while (text.charAt(j)==' ') j++;//pomijam pocz. spacje
            //dekoduje liczbe
            done=false;
        	while (!done) {
        		ch = text.charAt(j);
            	value1 = (int)( ch - '0' );
            	if (value1>=0 && value1<=9) {
            		x = 10*x + (int)( ch - '0' );
            		j++;
            		if (j>=text.length()) done=true;
            	} else done=true;
            }
            int[] wyn = {x, j};
            return wyn;
    }

    double[] readDoubleF(String text, int j) {
    	char ch;
    	double val1=0, val2=0;
    	int sign=0;

        while (text.charAt(j)==' ') j++;//pomijam pocz. spacje

    	int kk=text.indexOf(".",j+1);
        int ss=text.indexOf(" ",j+1);
        int zz=text.length();

        if (ss==-1) ss=zz;
        if (kk>0 && ss>kk) ss=kk;

        if (text.charAt(j)=='-') {
        	sign = -1;
        	j++;
        }
        for (int i = j; i < ss; i++ ) {
        	ch = text.charAt( i );
            val1 = 10 * val1 + (int)( ch - '0' );
        }

        ss=text.indexOf(" ",j+1);
        if (ss==-1) ss=zz;
        if (kk>0 && ss>kk) {
        	int ile = 0;
        	for (int i = kk+1; i < ss; i++ ) {
        		ch = text.charAt( i );
            	val2 = 10 * val2 + (int)( ch - '0' );
            	ile++;
        	}
        	val2 = val2/Math.pow(10,ile);
        }
        val1 += val2;
        if (sign==-1) val1 = -val1;
        double[] wyn = {val1, ss};
        return wyn;
    }

	void readData(String f_name) {
        FileInputStream fs;	// file stream
        BufferedReader br;	// class for reading data
     	String text;
     	int j;
     	int[] wyn = new int[2];

        try {
            // czyta plik
            fs = new FileInputStream( f_name );
            br = new BufferedReader( new InputStreamReader( fs ) );
			//czyta 1-szą linię i wyciąga parametry m,n,k
            text = br.readLine();
            j=0;
            wyn = readIntField(text,j);
            m = wyn[0];
            wyn = readIntField(text,wyn[1]);
            n = wyn[0];
            wyn = readIntField(text,wyn[1]);
            k = wyn[0];

            x = new double[m][n]; //inicjalizacja tablicy danych

        	//czyta pozostałe linie z danymi
            for (int i=0; i<m; i++) {
            	text = br.readLine();   // read next line
            	j=0;
            	double[] rwyn = {0., j};
            	while (text.charAt(j)==' ') j++;//pomijam pocz. spacje
            	for (int jj=0; jj<n; jj++) {
            		rwyn = readDoubleF(text, (int)rwyn[1]);
            		x[i][jj] = rwyn[0];
            	}
            }
            br.close();
        }
        catch( Exception e ) {
        	System.out.println("File "+f_name+" not opened "+e.toString());
        }
    }

    int getCard() {
    	return m;
    }

    int getDim() {
    	return n;
    }

    int getClust() {
    	return k;
    }

    double[][] getX() {
    	return x;
    }
}
