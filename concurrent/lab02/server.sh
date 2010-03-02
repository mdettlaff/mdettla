#!/bin/bash

while true
do
  number=`cat dane`
  if [ "$number" != "" ]
  then
    cat /dev/null > dane # czyszczenie bufora
    let result=number*number
    echo $result > wyniki
  fi
done
