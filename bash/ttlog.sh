#!/bin/bash

if [ $# -eq 0 ]
then
  echo Program zliczający unikalne IP zapisane w dzienniku ściągniętym ze strony
  echo http://www.szybkiepisanie.webpark.pl/ttlogstat.php
  echo Użycie: ./ttlog.sh ttlog.php.html
else
  records=$( cat $1 | tail -n +78 | head -n -5 | grep '<td>' \
      | awk '{ gsub(/.*?:[0-9][0-9]:[0-9][0-9]<\/td><td>/, ""); print }' \
      | awk '{ gsub(/<\/td><td><b>.*?/, ""); print }' \
      | wc -l )
  echo "wszystkich IP: $records"
  uniqrecords=$( cat $1 | tail -n +78 | head -n -5 | grep '<td>' \
      | awk '{ gsub(/.*?:[0-9][0-9]:[0-9][0-9]<\/td><td>/, ""); print }' \
      | awk '{ gsub(/<\/td><td><b>.*?/, ""); print }' \
      | sort -n | uniq \
      | wc -l )
  echo "unikalnych IP: $uniqrecords"
  echo -n "średnia ilość testów na unikalny IP: "
  echo "scale=2; $records/$uniqrecords" | bc

  cat $1 | tail -n +78 | head -n -5 | grep '<td>' \
	| awk '{ gsub(/.*?:[0-9][0-9]:[0-9][0-9]<\/td><td>/, ""); print }' \
	| awk '{ gsub(/<\/td><td><b>.*?/, ""); print }' \
	| sort -n | uniq -c > tmp.txt
  echo
  echo -n -e "IP z 1 testem:   ` cat tmp.txt | grep '^ *1 ' | wc -l `\t\t"
  echo "IP z 17 testami: ` cat tmp.txt | grep '^ *17 ' | wc -l `"
  echo -n -e "IP z 2 testami:  ` cat tmp.txt | grep '^ *2 ' | wc -l `\t\t"
  echo "IP z 18 testami: ` cat tmp.txt | grep '^ *18 ' | wc -l `"
  echo -n -e "IP z 3 testami:  ` cat tmp.txt | grep '^ *3 ' | wc -l `\t\t"
  echo "IP z 19 testami: ` cat tmp.txt | grep '^ *19 ' | wc -l `"
  echo -n -e "IP z 4 testami:  ` cat tmp.txt | grep '^ *4 ' | wc -l `\t\t"
  echo "IP z 20 testami: ` cat tmp.txt | grep '^ *20 ' | wc -l `"
  echo -n -e "IP z 5 testami:  ` cat tmp.txt | grep '^ *5 ' | wc -l `\t\t"
  echo "IP z 21 testami: ` cat tmp.txt | grep '^ *21 ' | wc -l `"
  echo -n -e "IP z 6 testami:  ` cat tmp.txt | grep '^ *6 ' | wc -l `\t\t"
  echo "IP z 22 testami: ` cat tmp.txt | grep '^ *22 ' | wc -l `"
  echo -n -e "IP z 7 testami:  ` cat tmp.txt | grep '^ *7 ' | wc -l `\t\t"
  echo "IP z 23 testami: ` cat tmp.txt | grep '^ *23 ' | wc -l `"
  echo -n -e "IP z 8 testami:  ` cat tmp.txt | grep '^ *8 ' | wc -l `\t\t"
  echo "IP z 24 testami: ` cat tmp.txt | grep '^ *24 ' | wc -l `"
  echo -n -e "IP z 9 testami:  ` cat tmp.txt | grep '^ *9 ' | wc -l `\t\t"
  echo "IP z 25 testami: ` cat tmp.txt | grep '^ *25 ' | wc -l `"
  echo -n -e "IP z 10 testami: ` cat tmp.txt | grep '^ *10 ' | wc -l `\t\t"
  echo "IP z 26 testami: ` cat tmp.txt | grep '^ *26 ' | wc -l `"
  echo -n -e "IP z 11 testami: ` cat tmp.txt | grep '^ *11 ' | wc -l `\t\t"
  echo "IP z 27 testami: ` cat tmp.txt | grep '^ *27 ' | wc -l `"
  echo -n -e "IP z 12 testami: ` cat tmp.txt | grep '^ *12 ' | wc -l `\t\t"
  echo "IP z 28 testami: ` cat tmp.txt | grep '^ *28 ' | wc -l `"
  echo -n -e "IP z 13 testami: ` cat tmp.txt | grep '^ *13 ' | wc -l `\t\t"
  echo "IP z 29 testami: ` cat tmp.txt | grep '^ *29 ' | wc -l `"
  echo -n -e "IP z 14 testami: ` cat tmp.txt | grep '^ *14 ' | wc -l `\t\t"
  echo "IP z 30 testami: ` cat tmp.txt | grep '^ *30 ' | wc -l `"
  echo -n -e "IP z 15 testami: ` cat tmp.txt | grep '^ *15 ' | wc -l `\t\t"
  echo "IP z 31 testami: ` cat tmp.txt | grep '^ *31 ' | wc -l `"
  echo -n -e "IP z 16 testami: ` cat tmp.txt | grep '^ *16 ' | wc -l `\t\t"
  echo "IP z 32 testami: ` cat tmp.txt | grep '^ *32 ' | wc -l `"
  rm tmp.txt
  echo
  echo "IP z co najmniej 30 testami:"
  cat $1 | tail -n +78 | head -n -5 | grep '<td>' \
      | awk '{ gsub(/.*?:[0-9][0-9]:[0-9][0-9]<\/td><td>/, ""); print }' \
      | awk '{ gsub(/<\/td><td><b>.*?/, ""); print }' \
      | sort -n | uniq -c | grep -e '[3-9,0][0-9] ' | sort -n
fi
