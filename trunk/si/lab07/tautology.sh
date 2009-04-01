#!/bin/bash

# Sprawdza czy podana formuła jest tautologią. Przykład:
# ~p => (p=>q);

tmpfile="/tmp/asdf.txt"

cd Sat
java -cp bin Glowny > $tmpfile
cd ..
# zmienna $? przechowuje kod wyjscia ostatnio uruchomionego programu
if [ $? -eq 0 ]
then
  ./tautologyCNF.py < $tmpfile
else
  echo Nieprawidlowa formula
fi
rm $tmpfile
