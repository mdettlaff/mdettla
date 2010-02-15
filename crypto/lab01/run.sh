echo "współczynnik niepodobieństwa rozkładu częstotliwości występowania liter
w zaszyfrowanym tekście do rozkładu w języku angielskim w ogóle:"
cat plain.txt | ./encrypt_subst.py | ./frequencies.py eng_frequencies.txt
