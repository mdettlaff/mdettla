//package spelnialnosc.rach.zdan;

public class Operator {
    private String  nazwa;
    private int  arnosc;
    
    Operator (String st) throws Blad {
        if (st.equals("~")) {
    	    this.nazwa = st;  this.arnosc = 1;
        }
        else  if (st.equals("&") || st.equals("V") ||
                  st.equals("=>") || st.equals("<=>")
		 ) {
    	    this.nazwa = st;  this.arnosc = 2;
        }
        else  throw new Blad("NIE MA TAKIEGO OPERATORA");
    }
   
    public String  operNapis () {
	return  nazwa;
    }
   
    public int  arnLiczba () {
	return  arnosc;
    }
 
}
