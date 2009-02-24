#!/bin/bash

./onp > tmp.txt
# zmienna $? przechowuje kod wyjscia ostatnio uruchomionego programu
if [ $? -eq 0 ]
then
  java Tautologia < tmp.txt
else
  echo Nieprawidlowa formula
fi
rm tmp.txt
