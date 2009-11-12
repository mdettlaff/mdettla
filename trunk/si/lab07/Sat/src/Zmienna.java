// package spelnialnosc.rach.zdan;

public class  Zmienna {
    private char ch;

    Zmienna (char ch) throws Blad {
	if (('a'<= ch && ch <= 'z') || ('A'<= ch && ch <= 'Z'))
	    this.ch = ch;
        else  throw new Blad("ZLA ZMIENNA");
    }

    public String  zmiennaNapis () {
	    char[]  napis = new char[1];
	    napis[0] = this.ch;
	    return  new String(napis);
    }

}
