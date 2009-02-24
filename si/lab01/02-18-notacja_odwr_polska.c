/*
 * ZAMIANA FORMULY NA ODWROTNA NOTACJE POLSKA
 * 
 * Gramatyka zrodlowa:
 * ~~~~~~~~~~~~~~~~~~~
 *  form0  ::=  form1  |  form '<=>' form1
 *  form1  ::=  form2  |  form2 '=>' form1
 *  form2  ::=  form3  |  form2 'V' form3
 *  form3  ::=  form4  |  form3 '&' form4
 *  form4  ::=  form5  |  '~' form4
 *  form5  ::=  'F'  |  'T'  |  zmienna  |  '(' form0 ')'
 * 
 * 
 * Gramatyka docelowa:
 * ~~~~~~~~~~~~~~~~~~~
 * onp     ::=  'F'  |  'T'  |  zmienna  |  onp '~'  |  onp onp 2-oper
 * 2-oper  ::=  '&'  |  'V'  |  '=>'  |  '<=>'
 */

//======================================================================

#include<stdio.h>
#include<ctype.h>
#include<stdlib.h>

typedef enum { FALSE = 0, TRUE = 1 }  Boolean;

typedef  enum {
  rownowaznosc = 0,
  implikacja = 1,
  alternatywa = 2,
  koniunkcja = 3,
  negacja = 4,
  falsz = 5,
  prawda = 6,
  l_nawias = 7,
  p_nawias = 8,
  srednik = 9
}  Znak_Leks;

typedef struct {
  Boolean czy_zmienna;
  Znak_Leks znak;  /* jesli  czy_zmienna == FALSE */
  char zm;         /* jesli  czy_zmienna == TRUE */
}  Leksem;

Boolean ok;
int nr_znaku;
char wprzod;
Leksem leks;

//======================================================================
// POMOCNICZE:

void  blad (char s[]) {
    // sygnalizacja b³êdu:
    // drukuje napis  s  a nastêpnie przerywa wykonanie programu
  int i;
  printf("\n");
  for (i=1; i<nr_znaku; i++)  printf(" ");
  printf ("^\n!!! BLAD:  %s !!!\n\n", s); exit(1);
}

//======================================================================
// LEKSYKALIA:

char  getznak () {
  int  z = getchar();
  if (z == '\n')  nr_znaku = 0;
  else  nr_znaku++; 
  return z;
}


void  nastleks() {
  while (isspace(wprzod))
    wprzod = getznak();
  switch (wprzod) {
    case 'V' : leks.czy_zmienna = FALSE; leks.znak = alternatywa; break;
    case '&' : leks.czy_zmienna = FALSE; leks.znak = koniunkcja; break;
    case '~' : leks.czy_zmienna = FALSE; leks.znak = negacja; break;
    case 'F' : leks.czy_zmienna = FALSE; leks.znak = falsz; break;
    case 'T' : leks.czy_zmienna = FALSE; leks.znak = prawda; break;
    case '(' : leks.czy_zmienna = FALSE; leks.znak = l_nawias; break;
    case ')' : leks.czy_zmienna = FALSE; leks.znak = p_nawias; break;
    case ';' : leks.czy_zmienna = FALSE; leks.znak = srednik; break;
    case '=' : wprzod = getznak();
      if (wprzod == '>') {
	leks.czy_zmienna = FALSE; leks.znak = implikacja;
      }
      else  blad ("=?");
      break;
    case '<' : wprzod = getznak();
      if (wprzod == '=') { wprzod = getznak();
	if (wprzod == '>') {
	  leks.czy_zmienna = FALSE; leks.znak = rownowaznosc;
        }
	else  blad ("<=?");
      }
      else  blad ("<?");
      break;
    default:
      if ('a' <= wprzod && wprzod <='z') {
	leks.czy_zmienna = TRUE; leks.zm = wprzod;
      }
      else  blad("dziwny znak");
      break;
    }
    wprzod = getznak();
  }

void  druk (Znak_Leks z) {
  switch (z) {
    case rownowaznosc : printf(" E"); break;
    case implikacja   : printf(" I"); break;
    case alternatywa  : printf(" V"); break;
    case koniunkcja   : printf(" &"); break;
    case negacja      : printf(" ~"); break;
    case falsz        : printf(" F"); break;
    case prawda       : printf(" T"); break;
    case l_nawias     : printf(" ("); break;
    case p_nawias     : printf(" )"); break;
    case srednik      : printf(" ;"); break;
  }
}

//======================================================================
// GLOWNA CZESC:


void  form0 ();
void  form1 ();
void  form2 ();
void  form3 ();
void  form4 ();
void  form5 ();


/* form0  ::=  form1 { '<==>' form1}* */
void  form0 () {
  form1();
  if (ok) {
    while (!leks.czy_zmienna && leks.znak == rownowaznosc) { nastleks();
      form1();
      if (ok)  druk(rownowaznosc);
      else  blad ("po znaku '<==>'");
    }
  }
}


/* form1  ::=  form2  |  form2 '==>' form1 */
void  form1 () {
  form2();
  if (ok) {
    if (!leks.czy_zmienna && leks.znak == implikacja) { nastleks();
      form1();
      if (ok)  druk(implikacja);
      else  blad ("po znaku '==>'");
    }
  }
}


/* form2  ::=  form3 { 'V' form3}* */
void  form2 () {
  form3();
  if (ok) {
    while (!leks.czy_zmienna && leks.znak == alternatywa) { nastleks();
      form3();
      if (ok)  druk(alternatywa);
      else  blad ("po znaku 'V'");
    }
  }
}


/* form3  ::=  form4 { '&' form4}* */
void  form3 () {
  form4();
  if (ok) {
    while (!leks.czy_zmienna && leks.znak == koniunkcja) { nastleks();
      form4();
      if (ok)  druk(koniunkcja);
      else  blad ("po znaku '&'");
    }
  }
}


/* form4  ::=   '~' form4  |  form5 */
void  form4 () {
  if (!leks.czy_zmienna && leks.znak == negacja) { nastleks();
    form4();
    if (ok)  druk(negacja);
    else  blad ("po znaku '~'");
  }
  else  form5();
}


/* form5  ::=  'F'  |  'T'  |  '(' form0 ')'  |  zmienna */
void  form5 () {
  if (!leks.czy_zmienna && leks.znak == falsz) { nastleks(); druk(falsz); } else
  if (!leks.czy_zmienna && leks.znak == prawda) { nastleks(); druk(prawda); } else
  if (!leks.czy_zmienna && leks.znak == l_nawias) { nastleks();
    form0();
    if (ok)
      if (!leks.czy_zmienna && leks.znak == p_nawias)  nastleks();
      else  blad ("brak ')'");
    else  blad ("po '('");
  }  else
  if (leks.czy_zmienna) { nastleks(); printf(" %c", leks.zm); }
  else  ok = FALSE;
}

//======================================================================
// INICJALIZACJE:

    
  void  inic () {
    ok = TRUE;
    nr_znaku = 0;
    wprzod = getznak();
    nastleks();
  }

//======================================================================

int main (void) {

  inic();
  form0();
  if (ok) {
    if (!leks.czy_zmienna || leks.znak == srednik) {
      printf (" ;\n");
      exit(0);
    }
    else  blad ("smieci na koncu");
  }
  else blad ("zla formula");
}

