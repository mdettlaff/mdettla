echo "Szyfr Vigenere'a"
plaintext="TOJESTBARDZOTAJNYTEKST"
key="TAJNE"
echo "tekst oryginalny:"
echo $plaintext
echo "tajny klucz:"
echo $key
echo "tekst zaszyfrowany:"
echo $plaintext | ./vigenere_encrypt.py $key
