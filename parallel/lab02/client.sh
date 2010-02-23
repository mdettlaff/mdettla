#!/bin/bash

read number
echo $number > dane
result=""
while [ "$result" == "" ]
do
  result=`cat wyniki`
done
cat /dev/null > wyniki # czyszczenie bufora
echo wynik: $result
