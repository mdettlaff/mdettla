#!/bin/bash

while true
do
  number=`cat dane`
  if [ "$number" != "" ]
  then
    cat /dev/null > dane # czyszczenie bufora
    result=$(($number*$number))
    echo $result > wyniki
  fi
done
