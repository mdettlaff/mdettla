sed -i 's/[^a-zA-Z]//g' $1
sed -i 's/\(.*\)/\U\1/' $1 # zamiana na wielkie litery
sed -i ':a;N;$!ba;s/\n//g' $1 # usunięcie znaków nowej linii
