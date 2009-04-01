//package spelnialnosc.rach.zdan;

import java.io.*;

public class Parsing {

    public class  Leksem {
	private boolean  czyzmienna;
	private String  znakleks;
	private Zmienna  zm;

	Leksem (String st) throws Blad {
	    if (st.equals("<=>") || st.equals("=>") || st.equals("V") ||
		st.equals("&") || st.equals("~") || st.equals("(") ||
		st.equals(")") || st.equals(";")
               ) {
		this.czyzmienna = false;  this.znakleks = st;
	    }
	    else  throw new Blad("ZLY OPERATOR-LEKSEM");
	}

	Leksem (Zmienna zm) throws Blad {
	    this.czyzmienna = true; this.zm = zm;
	}
	
	public boolean  leksCzyZmienna () { return czyzmienna; }
	public String  leksZnak () { return znakleks; }
	public Zmienna  leksZmienna () { return zm; }
	
	public String  leksemNapis () {
	    if (this.czyzmienna)  return  " " + this.zm.zmiennaNapis();
	    else  return  " " + this.znakleks;
	}
    }
    
    
    char  wprzod;
    Leksem  lk;


    private char  getznak (BufferedReader br) throws Blad {
    	try {
            char  zn;
    	    do { zn = (char)br.read(); }
    	    while (Character.isWhitespace(zn));
    	    return zn;
    	}
    	catch (IOException e) { throw new Blad("BLAD CZYTANIA ZNAKU"); }
    }
    
     
    private Leksem  nastleks (BufferedReader br) throws Blad {
        wprzod = getznak(br);
    	switch (wprzod) {
    	   case 'V' : lk = new Leksem("V"); break;
    	   case '&' : lk = new Leksem("&"); break;
    	   case '~' : lk = new Leksem("~"); break;
    	   case '(' : lk = new Leksem("("); break;
    	   case ')' : lk = new Leksem(")"); break;
    	   case ';' : lk = new Leksem(";"); break;
    	   case '=' :
    	       wprzod = getznak(br);
    	       if (wprzod == '>')  lk = new Leksem("=>");
    	       else  throw new Blad("=?");
    	       break;
    	   case '<' :
    	       wprzod = getznak(br);
    	       if (wprzod == '=') {
    		   wprzod = getznak(br);
    	           if (wprzod == '>')  lk = new Leksem("<=>");
    	           else  throw new Blad("<=?");
    	       }
    	       else  throw new Blad("<?");
    	       break;
    	   default :
    	       try { lk = new Leksem(new Zmienna(wprzod)); }
    	       catch (Blad e) { throw new Blad("DZIWNY ZNAK " + e.msg); }
    	}
    	return lk;
    }

    //======================================================================
    // GLOWNA CZESC PARSERA:


        /* form0  ::=  form1  |  form1 '<=>' form0 */
    public  Wyrazenie  form0 (BufferedReader br) throws Blad {
        Wyrazenie  w0, w1;
        w1 = form1(br);
        if (w1 != null) {
            if (!lk.leksCzyZmienna() && lk.leksZnak().equals("<=>")) {
            	lk = this.nastleks(br);
                w0 = form0(br);
                if (w0 == null)  throw new Blad("PO ZNAKU '<=>'");
                else {
                    Wyrazenie[] wyr = {w1, w0};
                    w1 = new Wyrazenie(new Operator("<=>"), wyr);
                }
            }
        }
        return w1;
    }


        /* form1  ::=  form2  |  form2 '=>' form1 */
    private  Wyrazenie  form1 (BufferedReader br) throws Blad {
        Wyrazenie  w1, w2;
        w2 = form2(br);
        if (w2 != null) {
            while (!lk.leksCzyZmienna() && lk.leksZnak().equals("=>")) {
                lk = nastleks(br);
                w1 = form1(br);
                if (w1 == null)  throw new Blad("PO ZNAKU '=>'");
                else {
                    Wyrazenie[] wyr = {w2, w1};
                    w2 = new Wyrazenie(new Operator("=>"), wyr);
                }
            }
        }
        return w2;
    }
  

        /* form2  ::=  form3 { 'V' form3}* */
    private  Wyrazenie  form2 (BufferedReader br) throws Blad {
        Wyrazenie  w0, w1;
        w0 = form3(br);
        if (w0 != null) {
            while (!lk.leksCzyZmienna() && lk.leksZnak().equals("V")) {
                lk = nastleks(br);
                w1 = form3(br);
                if (w1 == null)  throw new Blad("PO ZNAKU 'V'");
                else {
                    Wyrazenie[] wyr = {w0, w1};
                    w0 = new Wyrazenie(new Operator("V"), wyr);
                }
            }
        }
        return w0;
    }
    

        /* form3  ::=  form4 { '&' form4}* */
    private  Wyrazenie  form3 (BufferedReader br) throws Blad {
        Wyrazenie  w0, w1;
        w0 = form4(br);
        if (w0 != null) {
            while (!lk.leksCzyZmienna() && lk.leksZnak().equals("&")) {
                lk = nastleks(br);
                w1 = form4(br);
                if (w1 == null)  throw new Blad("PO ZNAKU '&'");
                else {
                    Wyrazenie[] wyr = {w0, w1};
                    w0 = new Wyrazenie(new Operator("&"), wyr);
                }
            }
        }
        return w0;
    }
    

        /* form4  ::=  '~' form4  |  form5 */
    private  Wyrazenie  form4 (BufferedReader br) throws Blad {
        Wyrazenie  w0;
        if (!lk.leksCzyZmienna() && lk.leksZnak().equals("~")) {
            lk = nastleks(br);
            w0 = form4(br);
            if (w0 == null)  throw new Blad("PO ZNAKU '~'");
            else {
                Wyrazenie[] wyr = {w0};
                w0 = new Wyrazenie(new Operator("~"), wyr);
            }
        }
        else  w0 = form5(br);
        return w0;
    }
    
    
        /* form5  ::=  zmienna  |  '(' form0 ')' */
    private  Wyrazenie  form5 (BufferedReader br) throws Blad {
        Wyrazenie  w0;
        if (lk.leksCzyZmienna()) {
            w0 = new Wyrazenie(lk.leksZmienna());
            lk = nastleks(br);
        }
        else  if (lk.leksZnak().equals("("))  {
            lk = nastleks(br);
            w0 = form0(br);
            if (w0 == null)  throw new Blad("PO ZNAKU '('");
            else {
                if (lk.leksZnak().equals(")"))  lk = nastleks(br);
                else  throw new Blad("BRAK ZNAKU ')'");
            }
        }
        else  w0 = null;
        return w0;
    } 
    
//======================================================================
    
    public void  initParse (BufferedReader  br) throws Blad {
    	lk = nastleks(br);
    }
    
}
