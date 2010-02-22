if [ "$1" == "" ]
then
  echo "Użycie: ./run.sh plik_tekstowy"
  exit
fi
echo "różnica między rozkładem częstotliwości występowania liter
w zaszyfrowanym tekście a rozkładem w języku angielskim w ogóle:"
cat $1 | ./encrypt_subst.py | ./frequencies.py frequencies_en.txt
echo "tekst odszfrowany za pomocą analizy częstości liter:"
cat $1 | ./encrypt_subst.py | ./decrypt_subst.py letters_by_frequency_en.txt
