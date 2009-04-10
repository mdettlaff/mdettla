Rozwiązanie zadania nr 2 do wykładu ze Sztucznej inteligencji.
Autor: Michał Dettlaff

Załączone programy, napisane w języku Python (wersja 2.5):
    autopilot.py - automatyczny pilot zaimplementowany jako sterownik rozmyty
    animation.py - animacja pokazująca lądowanie samolotu przez autopilota

Wykresy pokazujące zależność wysokości (h) od prędkości w kierunku dolnym (v):
    cog.png - dla wyostrzania zbioru wynikowego metodą środka ciężkości
    mom.png - dla wyostrzania zbioru wynikowego metodą średniego maksimum

Przykłady użycia:
Wypisanie h, v i wartości sterowania f w kolejnych iteracjach:
autopilot.py
Jak wyżej, ale z wyostrzaniem metodą średniego maksimum:
autopilot.py -m
Program animation.py działa na danych wyjściowych programu autopilot.py. Np.:
autopilot.py > output.txt
animation.py output.txt
