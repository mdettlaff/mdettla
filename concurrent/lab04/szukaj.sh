#!/bin/bash

if [ $# -lt 2 ]
then
  echo "Użycie: ./szukaj.sh plik katalog"
  exit 0
fi

needle=$1
haystack=$2
if [ "$3" == "" ]
then
  first_recursion_level=true
fi

# wypisujemy znalezione pliki i liczymy je
found_count=0
files=`ls -1 $haystack`
for file in $files
do
  if [ "$file" == "$needle" ]
  then
    echo "$haystack/$file"
    found_count=$[$found_count+1]
  elif [ -d $haystack/$file ]
  then
    $0 $needle $haystack/$file false &
    pids=("${pids[@]}" $!)
  fi
done
for pid in $pids
do
  wait $!
  found_count=$[$found_count+$?]
done

if [ ! $first_recursion_level ]
then
  exit $found_count
elif [ $found_count -eq 0 ]
then
  echo Nie znaleziono.
fi
