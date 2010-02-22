if [ "$1" == "" ]
then
  echo "Użycie: ./run.sh plik_tekstowy"
  exit
fi
echo "współczynnik niepodobieństwa rozkładu częstotliwości występowania liter
w zaszyfrowanym tekście do rozkładu w języku angielskim w ogóle:"
cat $1 | ./encrypt_subst.py | ./frequencies.py eng_frequencies.txt
