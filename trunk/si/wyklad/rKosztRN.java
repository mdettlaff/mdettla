/** Implementacja algorytmu zach³annego
 *  dla zadania poszukiwania najkrótszej drogi na mapie
 *  <Przyklad z ksi¹¿ki Russel & Norvig (1995)>
 */
 
import java.util.*;

class Node {
	int miasto, przez;
	double odl;
	Node(int city, int through, double dist) {
		miasto = city;
		przez  = through;
		odl    = dist;
	}
}

/* 
 * Strategia równomiernego kosztu
 */
class rKosztRN{
	String[] name = {"Ara", "Buc", "Cra", "Dob", "Efo",
	                 "Fag", "Giu", "Hir", "Ias", "Lug",
	                 "Meh", "Nea", "Ora", "Pit", "Rim",
	                 "Sib", "Tim", "Urz", "Vas", "Zer"};
	int n = name.length;
	int[][] odl = new int[n][n];
	LinkedList<Node> open = new LinkedList<Node>();
	LinkedList<Node> closed = new LinkedList<Node>();
	
	void init() {
		for (int i=0; i<n; i++) {
			for (int j=0; j<n; j++) 
			odl[i][j] = -1;
		}
		odl[12][19] =  71;
		odl[12][15] = 151;
		odl[ 0][19] =  75;
		odl[ 0][15] = 140;
		odl[ 0][16] = 118;
		odl[ 9][16] = 111;
		odl[ 9][10] =  70;
		odl[ 3][10] =  75;
		odl[ 2][ 3] = 120;
		odl[ 2][13] = 138;
		odl[ 2][14] = 146;
		odl[14][15] =  80;
		odl[ 5][15] =  99;
		odl[ 1][ 5] = 211;
		odl[13][14] =  97;
		odl[ 1][13] = 101;
		odl[ 1][ 6] =  90;
		odl[ 1][17] =  85;
		odl[ 7][17] =  98;
		odl[ 4][ 7] =  86;
		odl[17][18] = 142;
		odl[ 8][18] =  92;
		odl[ 8][11] =  87;
		
		for (int i=0; i<n; i++) {
			for (int j=i+1; j<n; j++) {
				odl[j][i] = odl[i][j];
			}
		}
	}
	
	int findBest(LinkedList lista) {
		Node nd;
		double dist = Double.MAX_VALUE;
		int tmp=0; //nr wezla o najmniejszej odlegloœci
		if (lista.size()==0) System.out.println("EMPTY LIST");
		else
		for (int i=0;i<lista.size(); i++) {
			nd = (Node)lista.get(i);
			if (nd.odl<dist) {
				dist=nd.odl;
				tmp =i;
			}
		}
		return tmp;
	}
	void run(int start_city, int end_city) {
		boolean done=false;
		int no,tmp;
		init();
		Node node = new Node(start_city,-1,0);
		Node node1; 
		double t_dist=0;
		open.add(node);
		System.out.println("START: "+name[start_city]+" STOP: "+name[end_city]);
		while (!done) {
			//System.out.println();
			if (open.size()==0) {
				System.out.println("FAILED!!!");
				done=true;
			} else {
				no = findBest(open);
				node = open.get(no);
				System.out.print("Rozwijam wezel: "+name[node.miasto]);
				if (node.przez>-1) {
					System.out.println("("+name[node.przez]+")");
				} else System.out.println();
				closed.add(node);
				open.remove(no);
				if (node.miasto==end_city) {
					System.out.println("KONIEC ");
					piszTrase(end_city, start_city,node.odl);
					done=true;
				} else {
					for (int j=0; j<odl.length; j++) {
						if (odl[node.miasto][j]>0) {
							t_dist = node.odl+odl[node.miasto][j];
							//czy istnieje "lepsza" droga przez j
							tmp = better(j,t_dist);
							if (tmp==(int)Double.MAX_VALUE) {
								node1=new Node(j,node.miasto,t_dist);
								open.add(node1);
							} else
							if (tmp>=0 && tmp<=odl.length) {
								open.remove(tmp);
								node1=new Node(j,node.miasto,t_dist);
								open.add(node1);
							} 
						}
					}
				}
				/*
				 *UWAGA: ponizsze dwie linie mozna
				 *odkomentowac
				 */
				pisz("Lista OPEN:",open);
				pisz("Lista CLOSED:",closed);
				System.out.println();
			}	
		}
	}
	
	/*
	 * Sprawdza, czy s¹siad rozwijanego wêz³a jest lepszy
	 * od ju¿ istniej¹cego na liœcie open.
	 * WE: city - kiandydat do umieszczenia na liœcie
	 *     dist - jego odleg³oœæ od wêz³a starowego
	 * WY: tmp =-2 - kandydatk ju¿ wizytowsany
	 *     tmp =-1 - ju¿ jest takie miasto i lepsze
	 *     tmp =MAX_VALUE - nie ma kandydata na liœcie open 
	 *               (tzn. wstawiæ go bezwarunkowo)
	 *     tmp \in {0, dist.length} - nale¿y tê pozycjê
	 *                zast¹piæ kandydatem
	 */
	int better(int city, double dist) {
		boolean found=false;
		int tmp=-1;
		Node nd;
		for (int j=0; j<closed.size(); j++) {
			nd = (Node)closed.get(j);
			if (nd.miasto == city) {
				if (nd.odl > dist) tmp=(int)Double.MAX_VALUE;
				return tmp;
			}
		}
		for (int j=0; j<open.size(); j++) {
			nd = (Node)open.get(j);
			if (nd.miasto == city) {
				found = true;
				if (nd.odl>dist) tmp = j;
				return tmp;
			}
		}
		if (!found) tmp=(int)Double.MAX_VALUE;
		return tmp;
	}
	
	/* Wypisuje trasê zaczynaj¹c od wêz³a koñcowego 
	 * koñcz¹c na startowym*/
	void piszTrase(int city, int start, double dist) {
		boolean done=false, found;
		int i;
		Node node;
		if (closed.size()==0) System.out.println("EMPTY LIST");
		else {
			System.out.println("Total length: "+dist);
			System.out.println("Route: ");
			System.out.println("   "+name[city]);
			while (city!=start) {
				found = false;
				i = 0;
				do {
					node = closed.get(i);
					if (node.miasto==city) {
						found=true;
						city = node.przez;
						//if (city>-1) 
						System.out.println("   "+name[city]);
					} 
					i++;
				} while (!found);
			}
		}
	}
	
	public static void main(String[] args) {
		new rKosztRN().run(0,1);
	}
	
	void piszTab() {
		for (int i=0; i<odl.length; i++) {
			System.out.print(name[i]+": ");
			for (int j=0; j<odl.length; j++) {
				if (odl[i][j]>-1) {
					System.out.println(name[j]+"("+odl[i][j]+") ");
				} else System.out.print(") "+odl[i][j]+") ");
			}
			System.out.println();
		}
	}
	
	/* 
	 * Wypisuje zawartoœæ listy LISTA
	 * s - nag³ówek nadawany listingowi
	 */
	void pisz(String s,LinkedList lista) {
		Node nd;
		System.out.println(s+" ");
		if (lista.size()==0) System.out.println("EMPTY LIST");
		else
		for (int i=0;i<lista.size(); i++) {
			nd = (Node)lista.get(i);
			System.out.print("   "+name[nd.miasto]+"(");
			if (nd.przez>=0) System.out.println(name[nd.przez]+") "+nd.odl);
			else
			System.out.println(") "+nd.odl);
		}
	}
	
    void pressAKey() {
		System.out.println("Press ENTER");
		try {
      		System.in.read();
		} catch(Exception e) {}
	}
}