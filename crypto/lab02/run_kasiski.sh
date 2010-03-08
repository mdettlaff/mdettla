echo "Szukanie długości klucza metodą Kasiskiego"
pattern_len=4
key="PIANO"
echo "tajny klucz:"
echo $key
echo "szukamy odległości między zbitkami liter o długości $pattern_len"
cat plain_en.txt | ./encrypt_vigenere.py $key | ./kasiski.py $pattern_len
