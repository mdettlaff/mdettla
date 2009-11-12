// package spelnialnosc.rach.zdan;

class KoniunktywnaPostacNormalna {

    private Wyrazenie elimRown (Wyrazenie w) throws Blad {
            //  P <=> Q  -->  (P => Q) & (Q => P)
	if (w.czyZmienna())  return w;
	else {
	    Operator op = w.oper();
	    Wyrazenie[] wyr = new Wyrazenie[w.argum().length];
	    System.arraycopy(w.argum(),0, wyr,0, wyr.length);
	    for (int i=0; i<wyr.length; i++)
	        wyr[i] = elimRown(wyr[i]);
	    if (op.operNapis().equals("<=>")) {
	        Wyrazenie[] wyr1 = new Wyrazenie[2];
	        Wyrazenie w0, w1;
	        wyr1[0] = wyr[1];  wyr1[1] = wyr[0];
	        w0 = new Wyrazenie(new Operator("=>"), wyr);
	        w1 = new Wyrazenie(new Operator("=>"), wyr1);
	        wyr[0] = w0;  wyr[1] = w1;  op = new Operator("&");
	    }
	    return  new Wyrazenie(op, wyr);
	}
    }


    private Wyrazenie elimImpl (Wyrazenie w) throws Blad {
            //  P => Q  -->  ~P V Q
        if (w.czyZmienna())  return w;
        else {
            Operator op = w.oper();
            Wyrazenie[] wyr = new Wyrazenie[w.argum().length];
            System.arraycopy(w.argum(),0, wyr,0, wyr.length);
            for (int i=0; i<wyr.length; i++)
                wyr[i] = elimImpl(wyr[i]);
            if (op.operNapis().equals("=>")) {
                Wyrazenie[] wyr1 = { wyr[0] };
                wyr[0] = new Wyrazenie(new Operator("~"), wyr1);
                op = new Operator("V");
            }
            return  new Wyrazenie(op, wyr);
        }
    }


    private Wyrazenie deMorgan (Wyrazenie w) throws Blad {
        if (w.czyZmienna())  return w;
        else {
            Wyrazenie w0;
            Operator op = w.oper();
            Wyrazenie[] wyr = new Wyrazenie[w.argum().length];
            System.arraycopy(w.argum(),0, wyr,0, wyr.length);
            if (op.operNapis().equals("~")) {
                if (wyr[0].czyZmienna())  
                    return w;
                else {
                    Operator op1 = wyr[0].oper();
                    Wyrazenie[] wyr1 = new Wyrazenie[wyr[0].argum().length];
                    System.arraycopy(wyr[0].argum(),0, wyr1,0, wyr1.length);
                    if (op1.operNapis().equals("V")) {
			                //  ~(P V Q)  -->  ~P & ~Q
                        Wyrazenie[][] wyr2 = new Wyrazenie[wyr1.length][1];
                        Wyrazenie[] wyr0 = new Wyrazenie[wyr1.length];
                        for (int i=0; i<wyr1.length; i++) {
                            wyr2[i][0] = wyr1[i];
                            wyr0[i] = new Wyrazenie(new Operator("~"), wyr2[i]);
                        }
                        w0 = new Wyrazenie(new Operator("&"), wyr0);
                    }
                    else  if (op1.operNapis().equals("&")) {
                            //  ~(P & Q)  -->  ~P V ~Q
                        Wyrazenie[][] wyr2 = new Wyrazenie[wyr1.length][1];
                        Wyrazenie[] wyr0 = new Wyrazenie[wyr1.length];
                        for (int i=0; i<wyr1.length; i++) {
                            wyr2[i][0] = wyr1[i];
                            wyr0[i] = new Wyrazenie(new Operator("~"), wyr2[i]);
                        }
                        w0 = new Wyrazenie(new Operator("V"), wyr0);
                    }
                    else  if (op1.operNapis().equals("~")) {
                            //  ~~P  -->  P
                        w0 = wyr1[0];
                    }
                    else  w0 = new Wyrazenie(new Operator("~"), wyr);
                }
            }
            else  w0 = new Wyrazenie(w.oper(), wyr);
            if (w0.czyZmienna())  return w0;
            else {
                Wyrazenie[] wyr2 = new Wyrazenie[w0.argum().length];
                for (int i=0; i<wyr2.length; i++)
                    wyr2[i] = deMorgan(w0.argum()[i]);
                return new Wyrazenie(w0.oper(), wyr2);
            }
        }
    }


    private Wyrazenie rozdziel (Wyrazenie w) throws Blad {
        if (w.czyZmienna())  return w;
        else {
            Operator op = w.oper();
            Wyrazenie[] wyr = new Wyrazenie[w.argum().length];
            for (int i=0; i<wyr.length; i++)
                wyr[i] = rozdziel(w.argum()[i]);
            if (op.operNapis().equals("V")) {
                if (! wyr[0].czyZmienna() &&
                    wyr[0].oper().operNapis().equals("&")
                   ) {
                        //  (P & Q) V R  -->  (P V R) & (Q V R)
                    Wyrazenie w1 = wyr[0].argum()[0];
                    Wyrazenie w2 = wyr[0].argum()[1];
                    Wyrazenie w3 = wyr[1];
                    Wyrazenie[] wyr0 = {w1, w3};
                    Wyrazenie[] wyr1 = {w2, w3};
                    Wyrazenie[] wyr2 = {
                        rozdziel(new Wyrazenie(new Operator("V"), wyr0)) ,
                        rozdziel(new Wyrazenie(new Operator("V"), wyr1))
                    };
                    return  new Wyrazenie(new Operator("&"), wyr2);
                }
                else if (! wyr[1].czyZmienna() &&
                         wyr[1].oper().operNapis().equals("&")
                        ) {
                        //  P V (Q & R)  -->  (P V Q) & (P V R)
                    Wyrazenie w1 = wyr[0];
                    Wyrazenie w2 = wyr[1].argum()[0];
                    Wyrazenie w3 = wyr[1].argum()[1];
                    Wyrazenie[] wyr0 = {w1, w2};
                    Wyrazenie[] wyr1 = {w1, w3};
                    Wyrazenie[] wyr2 = {
                        rozdziel(new Wyrazenie(new Operator("V"), wyr0)) ,
                        rozdziel(new Wyrazenie(new Operator("V"), wyr1))
                    };
                    return  new Wyrazenie(new Operator("&"), wyr2);
                }
                else  return new Wyrazenie(new Operator("V"), wyr);
            }
            else  return new Wyrazenie(op, wyr);
        }
    }


    private Wyrazenie porzadek (Wyrazenie w) throws Blad {
        if (w.czyZmienna())  return w;
        else {
            Operator op = w.oper();
            Wyrazenie[] wyr = new Wyrazenie[w.argum().length];
            for (int i=0; i<wyr.length; i++)
                wyr[i] = porzadek(w.argum()[i]);
            if (op.operNapis().equals("V")) {
                if (! wyr[0].czyZmienna() &&
                    wyr[0].oper().operNapis().equals("V")
                   ) {
                        //  (P V Q) V R  -->  P V (Q V R)
                    Wyrazenie w1 = wyr[0].argum()[0];
                    Wyrazenie w2 = wyr[0].argum()[1];
                    Wyrazenie w3 = wyr[1];
                    Wyrazenie[] wyr1 = {w2, w3};
                    Wyrazenie[] wyr2 = {
                        porzadek(w1) ,
                        porzadek(new Wyrazenie(new Operator("V"), wyr1))
                    };
                    return  new Wyrazenie(new Operator("V"), wyr2);
                }
                else  return new Wyrazenie(new Operator("V"), wyr);
            }
            else  if (op.operNapis().equals("&")) {
                if (! wyr[0].czyZmienna() &&
                    wyr[0].oper().operNapis().equals("&")
                   ) {
                        //  (P & Q) & R  -->  P & (Q & R)
                    Wyrazenie w1 = wyr[0].argum()[0];
                    Wyrazenie w2 = wyr[0].argum()[1];
                    Wyrazenie w3 = wyr[1];
                    Wyrazenie[] wyr1 = {w2, w3};
                    Wyrazenie[] wyr2 = {
                        porzadek(w1) ,
                        porzadek(new Wyrazenie(new Operator("&"), wyr1))
                    };
                    return  new Wyrazenie(new Operator("&"), wyr2);
                }
                else  return new Wyrazenie(new Operator("&"), wyr);
            }
            else  return new Wyrazenie(op, wyr);
        }
    }


    public Wyrazenie przeksztalc (Wyrazenie w) throws Blad {
        w = elimRown(w);
        w = elimImpl(w);
        w = deMorgan(w);
        w = rozdziel(w);
        w = porzadek(w);
        return w;
    }

}
