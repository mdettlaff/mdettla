#!/bin/bash

#=============================================================================#
# autor: Micha³ Dettlaff                                                      #
# Skrypt zapisuj±cy kod ¼ród³owy w C do pliku HTML z kolorowaniem sk³adni     #
# oraz automatycznym utworzeniem wciêæ.                                       #
# u¿ycie: ./c2html < plik_wejsciowy.c > plik_wyjsciowy.html                   #
#=============================================================================#

BR="<br>"

wypisz_poczatek() {
  poczatek='<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">'
  echo "$poczatek"
  echo "<html>"
  echo "<head>"
  echo "</head>"
  echo "<body>"
  echo
}

przetwarzaj_linie() {
  szerokosc_tabulacji=4
  margines=0
  przesunieto_marg=0 # ile razy przesunieto margines dla blokow 1-linijkowych

  while read -r wiersz # czyta po jednej linijce ze standardowego wejscia
  do
    # konwersja nawiasow '<' i '>'
    echo "$wiersz" | grep -q -e '<' -e '>'
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" | sed -e 's/</\&lt;/g' -e 's/>/\&gt;/g' )
      wiersz="$temp"
    fi

    # komentarze dla include i define
    if [[ "$wiersz" =~ "#" ]]
    then
      temp=$( echo "$wiersz" |
	sed -e "s/#/<font color=darkviolet>#/" |
	sed -e "a</font>" )
      wiersz="$temp"
    fi

    # komentarze jednolinijkowe
    echo "$wiersz" | grep -q "\/\/"
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" |
	sed -e "s/\/\//<font color=blue>\/\//" |
	sed -e "a</font>" )
      wiersz="$temp"
    fi

    # komentarze wielolinijkowe
    echo "$wiersz" | grep -q "\/\*" # poczatek komentarza
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" |
	sed -e "s/\/\*/<font color=blue>\/\*/" )
      wiersz="$temp"
    fi
    echo "$wiersz" | grep -q "\*\/"
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" |
	sed -e "s/\*\//\*\/<\/font>/" )
      wiersz="$temp"
    fi

    # liczby
    echo "$wiersz" | grep -q "[0-9]"
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" |
	awk '{
	  while (match($0, /[[(,.\/\*%\+\-= ][0-9]+/) > 0)
	    $0 = substr($0, 1, RSTART) "<font color=red>" \
	    substr($0, RSTART+1, RLENGTH-1) "</font>" \
	    substr($0, RSTART+RLENGTH);
	  print }
	' )
      wiersz="$temp"
    fi

    # stringi
    if [[ "$wiersz" =~ "\"" ]]
    then
      temp=$( echo "$wiersz" |
	awk '{ gsub(/".*?"/, "<font color=red>&</font>"); print }' )
      wiersz="$temp"
    fi

    # char-y w nawiasach `'`
    slowo="'"
    echo "$wiersz" | grep -q -e "$slowo"
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      temp=$( echo "$wiersz" |
	sed -e "s/ $slowo/ <font color=red>$slowo/g" |
	sed -e "s/=$slowo/=<font color=red>$slowo/g" |
	sed -e "s/$slowo /$slowo<\/font> /g" |
	sed -e "s/$slowo;/$slowo<\/font>;/g" |
	sed -e "s/$slowo)/$slowo<\/font>)/g" )
      wiersz="$temp"
    fi
    echo "$wiersz" | grep -q -e "$slowo$" # jesli `'` na koncu wiersza
    if [ "$?" -eq 0 ]
    then
      temp=$( echo "$wiersz" |
	sed -e "a<\/font>" )
      wiersz="$temp"
    fi

    # kolorowanie slow kluczowych dotyczacych zmiennych na zielono
    slowa_kluczowe="double char const extern float int long short \
	    signed sizeof static struct typedef unsigned void volatile"
    for slowo in $slowa_kluczowe
    do
      echo "$wiersz" | grep -q "$slowo "
      if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
      then
	temp=$( echo "$wiersz" |
	  sed -e "s/$slowo /<font color=green>$slowo<\/font> /g" )
	wiersz="$temp"
      fi
    done

    # kolorowanie instrukcji sterujacych na brazowo
    slowa_kluczowe="break case continue default else for goto if \
	    return switch while"
    for slowo in $slowa_kluczowe
    do
      echo "$wiersz" | grep -q -e "$slowo " -e "$slowo("
      if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
      then
	temp=$( echo "$wiersz" |
	  sed -e "s/$slowo /<font color=brown>$slowo<\/font> /g" |
	  sed -e "s/$slowo(/<font color=brown>$slowo<\/font>(/g" )
	wiersz="$temp"
      fi
    done

    # kolorowanie z gory ustalonych stalych na czerwono
    slowa_kluczowe="BUFSIZ EOF NULL SIGINT"
    for slowo in $slowa_kluczowe
    do
      echo "$wiersz" | grep -q -e "$slowo"
      if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
      then
	temp=$( echo "$wiersz" |
	  sed -e "s/$slowo/<font color=red>$slowo<\/font>/g" )
	wiersz="$temp"
      fi
    done

    # kolorowanie roznego rodzaju znakow specjalnych
    slowa_kluczowe="\\\\n \\\\t \\\\e \\\\\" %d %c %f %s"
    for slowo in $slowa_kluczowe
    do
      echo "$wiersz" | grep -q -e "$slowo"
      if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
      then
	temp=$( echo "$wiersz" |
	  sed -e "s/$slowo/<font color=darkviolet>$slowo<\/font>/g" )
	wiersz="$temp"
      fi
    done


    # obliczanie przesuniecia marginesu z lewej strony
    if [[ "$wiersz" =~ "}" ]]
    then
      margines=$margines-$szerokosc_tabulacji
    fi

    # non K&R style
    if [[ "$przesunieto_marg" > 0 && "$wiersz" =~ "{" ]]
    then
      margines=$margines-$szerokosc_tabulacji
      tymczasowe_cofniecie=1
    fi

    #=============================================#
    # wypisujemy margines oraz przetworzona linie #
    #=============================================#
    for ((i=1; i <= $[margines/2]; i++))
    do
      echo -n "&nbsp; "
    done
    echo -n "$wiersz"
    echo $BR

    # non K&R style
    if [[ "$tymczasowe_cofniecie" > 0 ]]
    then
      margines=$margines+$szerokosc_tabulacji
      tymczasowe_cofniecie=0
    fi

    # obliczanie przesuniecia marginesu z lewej strony
    if [[ "$wiersz" =~ "{" ]]
    then
      margines=$margines+$szerokosc_tabulacji
    fi

    # przesuniecie dla blokow jednolinijkowych po while, for, if etc.
    echo "$wiersz" | grep -q -e 'for' -e 'if' -e 'else' -e 'while'
    if [ "$?" -eq 0 ] # jesli pasuje do wyrazenia z grepa
    then
      echo "$wiersz" | grep -q '{'
      if [ "$?" -ne 0 ] # jesli pasuje do wyrazenia z grepa
      then # mamy do czynienia z jednolinijkowym blokiem
	margines=$margines+$szerokosc_tabulacji
	przesunieto_marg=$przesunieto_marg+1
      fi
    else
      if [[ "$przesunieto_marg" > 0 ]]
      then
	let margines=margines-szerokosc_tabulacji*przesunieto_marg
	przesunieto_marg=0
      fi
    fi

  done # koniec while
}

wypisz_koniec() {
  echo
  echo "</body>"
  echo "</html>"
}

# g³ówne cia³o programu
wypisz_poczatek
przetwarzaj_linie
wypisz_koniec

