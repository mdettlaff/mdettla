/*
 * ELIMINACJA IMPLIKACJI I ROWNOWAZNOSCI
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
 *  tymcz0  ::=  tymcz1  |  tymcz0 'V' tymcz1
 *  tymcz1  ::=  tymcz2  |  tymcz1 '&' tymcz2
 *  tymcz2  ::=  tymcz3  |  '~' tymcz2
 *  tymcz3  ::=  zmienna  |  '(' tymcz0 ')'
 */


class Elim {

}
