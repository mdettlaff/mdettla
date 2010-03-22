echo "Szyfrowanie i kryptoanaliza szyfru Vigenere'a"
key="PIANO"
echo "tajny klucz:"
echo $key
cat plain_en.txt | ./vigenere_encrypt.py $key | ./vigenere_cryptanalysis.py
