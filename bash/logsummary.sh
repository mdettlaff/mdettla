# Wypisuje ilosc commitow, jakie wykonali poszczegolni uczestnicy projektu
svn log | egrep "^r[0-9]+ " | cut -f2 -d'|' | sed 's/-guest//' | sort | uniq -c | sort -n -r
