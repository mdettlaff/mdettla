Rozwiązanie zadania nr 1 do wykładu ze Sztucznej inteligencji.
Autor: Michał Dettlaff

Wyniki eksperymentów oraz ich interpretacja zawarte są w pliku results.txt
Wykres obrazujący wartość dopasowania najlepszego osobnika w populacji
w kolejnych iteracjach znajduje się w pliku graph.png

Załączone programy, napisane w języku Python (wersja 2.5):
ga.py - algorytm genetyczny, selekcja proporcjonalna oraz turniejowa
a_star.py - algorytm A*
animation.py - wizualizacja działania algorytmu genetycznego

Aby uruchomić program (kompilacja nie jest wymagana), należy użyć polecenia:
python nazwa_programu.py

Python jest zainstalowany domyślnie na większości platform linuksowych.
Wersję dla systemu Windows można pobrać ze strony:
http://www.python.org/ftp/python/2.5.4/python-2.5.4.msi

Przykłady użycia:
python ga.py maze.txt # pojedynczy przebieg algorytmu genetycznego (epoka)
python ga.py maze.txt 10 # obliczenie uśrednionej ilości iteracji dla 10 epok
python ga.py -s 50 maze.txt 10 # ustawienie liczebności populacji na 50
python ga.py -t 4 maze.txt 10 # selekcja turniejowa o rozmiarze turnieju 4
python a_star.py maze.txt
python animation.py maze.txt

Uwaga: do uruchomienia programu animation.py wymagana jest biblioteka PyQt4.
Wersja dla systemu Windows jest dostępna pod adresem:
http://www.riverbankcomputing.co.uk/static/Downloads/PyQt4/PyQt-Py2.5-gpl-4.4.3-1.exe
