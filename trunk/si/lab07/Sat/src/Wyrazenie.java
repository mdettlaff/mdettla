// package spelnialnosc.rach.zdan;

public class Wyrazenie {
    private boolean czyzm;
    private Zmienna zm;
    private Operator op;
    private Wyrazenie[] wyr;


    public Wyrazenie (Zmienna zm) { this.czyzm = true;  this.zm = zm; }


    public Wyrazenie (Operator op, Wyrazenie[] wyr) throws Blad {
      	this.czyzm = false;
        this.op = op;
        if (op.arnLiczba() == wyr.length) {
        	for (int i=0; i<wyr.length; i++)
        		if (wyr[i] == null)
        			throw new Blad("WYRAZENIE: argument " + i + " nullowy");
            this.wyr = new Wyrazenie[wyr.length];
            System.arraycopy(wyr,0, this.wyr,0, wyr.length);
        }
        else  throw new Blad("WYRAZENIE: zla liczba argumentow");
    }


    public boolean  czyZmienna () {
	return  this.czyzm;
    }


    public Operator  oper () {
	return  this.op;
    }


    public Wyrazenie[]  argum () {
	Wyrazenie[] wyr = new Wyrazenie[this.wyr.length];
	System.arraycopy(this.wyr,0, wyr,0, wyr.length);
	return  wyr;
    }


    public String  wyrNapis () {
        if (this.czyzm)  return zm.zmiennaNapis();
        else {
            String  st = "(";
            if (this.wyr.length == 1) {
                st = st + this.op.operNapis();
                st = st + " " + this.wyr[0].wyrNapis();
            }
            else {
                st = st + this.wyr[0].wyrNapis();
                st = st + " " + this.op.operNapis();
                st = st + " " + this.wyr[1].wyrNapis();
            }
            st = st + ")";
            return st;
        }
    }

}
