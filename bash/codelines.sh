# Skrypt liczący linie kodu źródłowego w danym katalogu

tmpfile='/tmp/xyz.txt'

if [ $# -le 0 ]
then
  echo Użycie: $0 sciezka_do_kodu_zrodlowego
else
  if [ -d $1 ]
  then
  echo -n "Ilość linii kodu: "
  find $1 \
    \( -name '*.c' -or -name '*.h' -or -name '*.java' \) \
    -exec grep -c '.*' '{}' \; \
    > $tmpfile
  awk '{ lines=lines+$0 }; END { printf("%d\n", lines); }' $tmpfile
  rm $tmpfile
  else
    echo Nie znaleziono katalogu
  fi
fi
