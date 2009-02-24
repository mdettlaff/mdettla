//                                          pmp@math.univ.gda.pl  2006
// budowanie i drukowanie drzew cz-cz z wartownikiem wspolnym
// dla wszystkich wezlow
/* ------------  implementacja ------------------------------------- */

#include "rbtree.h"

char wydruk[IL_POZ+1][SZER_EKR];

/* funkcja inicjujaca nil; musi byc wywolana przed budowaniem drzewa */
/* zauwaz, ze nil wskazuje sam na siebie */
void nilInit(void){
  nil= (Wskwezla) malloc(sizeof(Twezla));
  nil->p = nil;
  nil->kolor = BLACK;
  nil->left = nil->right = nil;  
}

void drukujost(Wskwezla w, int l, int p,int poziom){
       int srodek = (l+p)/2;
       if (w==nil)   return; 
       wydruk[poziom][srodek]='*';
}

void drukujwew(Wskwezla w, int l, int p,int poziom){
       int srodek = (l+p)/2;
       int i,dl;
       char s[9];
       if (w==nil)    return;
       if (w->kolor==BLACK)
         dl=sprintf(s,"%d",w->klucz);
       else
         dl=sprintf(s,"%+d-",w->klucz);
       for (i=0;i<dl;i++)
         wydruk[poziom][srodek-dl/2+i]=s[i];
       if (++poziom<IL_POZ){
         drukujwew(w->left,l,srodek,poziom) ;
         drukujwew(w->right,srodek+1,p,poziom) ;
       }
       else {
         drukujost(w->left,l,srodek,poziom) ;
         drukujost(w->right,srodek+1,p,poziom) ;
       }
}

void drukuj(Wskwezla w){
  int j,i;
  for (i=0;i<=IL_POZ;i++)
    for (j=0;j<SZER_EKR;j++)
      wydruk[i][j] = ' ';
  drukujwew(w,0,SZER_EKR,0);
  for (i=0;i<=IL_POZ;i++){
      for (j=0;j<=SZER_EKR;j++)
	if (wydruk[i][j] == '+'){
          printf("\e[40;31;1m");
	  putchar(' ');
	}
        else if (wydruk[i][j] == '-'){
          printf("\e[0m");
	  putchar(' ');
	}
        else
	  putchar(wydruk[i][j]);
      printf("\n");
  }
}

/* 2 do potegi n - fun. pomocnicza */
int potega(int n){
	if (n==0)  return 1;
	else       return 2*potega(n-1) ;
}

Wskwezla buduj1(int n, int przesuniecie, Wskwezla p){
    Wskwezla w = (Wskwezla) malloc(sizeof(Twezla));
    if (n==0)   w=nil;
    else {
    w->p=p;
    w->klucz = (potega(n-1)+przesuniecie)*1;
    w->kolor = (n%2 ? RED : BLACK);
    w->left = buduj1(n-1,przesuniecie,w);
    w->right = buduj1(n-1,przesuniecie+potega(n-1),w);
    }
    return w;
}
Wskwezla buduj(int n){
  Wskwezla w = buduj1(n+1,0,nil);
  w->kolor=BLACK;
  return w;
}
