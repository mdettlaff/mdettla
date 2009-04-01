//package spelnialnosc.rach.zdan;

import java.io.*;

class Glowny {

    public static void main(String[] args) {
        try {
	    InputStreamReader  inp = new InputStreamReader(System.in);
            BufferedReader  br = new BufferedReader(inp);
            Parsing pars = new Parsing();
            KoniunktywnaPostacNormalna kpn = new KoniunktywnaPostacNormalna();
            Wyrazenie w;
            pars.initParse(br);
	    w = pars.form0(br);
	    w = kpn.przeksztalc(w);
            if (pars.lk.leksZnak().equals(";"))
                System.out.println("\n" + w.wyrNapis() + "\n");
	    else  throw new Blad("SMIECI NA KONCU");
        }
	catch (Blad e) { System.err.println(e.msg); }
    }
}
