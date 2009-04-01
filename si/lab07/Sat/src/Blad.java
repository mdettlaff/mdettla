//package spelnialnosc.rach.zdan;

public class  Blad  extends Exception {
    String msg;
    private static final long serialVersionUID = 1L;
    Blad (String msg) { this.msg = "\n!!! " + msg + " !!!\n"; }
}
