#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Szukanie optymalnego układu klawiatury za pomocą algorytmu genetycznego.

Stosujemy algorytm genetyczny z selekcją turniejową.
Genotypem pojedynczego osobnika będzie ciąg znaków reprezentujący układ
klawiszy na klawiaturze. Przykładowo, dla układu QWERTY wygląda on tak:
qwertyuiopasdfghjkl;zxcvbnm,.?
Każdy znak może oczywiście wystąpić tylko raz.

Mutację definiujemy jako zamianę dwóch znaków miejscami. Prawdopodobieństwo
jednej mutacji dla osobnika wynosi 50%, dwóch mutacji: 25%, trzech 12.5% itd.
Stosujemy krzyżowanie wielopunktowe z prawdopodobieństwem 0.7.

Funkcja oceny (przystosowania) obliczana jest za pomocą wzoru
c1*w1 + c2*w2 + ... + cN * wN,
gdzie c1,...,cN oznaczają cechy układu klawiatury, a w1,...,wN to ich wagi,
decydujące o wpływie danej cechy na ogólną ocenę. Im mniejsza wartość funkcji
oceny, tym lepsze przystosowanie osobnika. Wartości liczbowe są przypisywane
cechom na podstawie analizy reprezentatywnego zbioru tekstów.
Pod uwagę brane są następujące cechy:
    1. Rząd klawiszy. Za literę w górnym rzędzie wartość zwiększana jest o 1,
       a w dolnym o 2. Ruch palca wskazującego do środka: +1.
    2. Bliskość klawiszy. Za literę wpisaną tym samym palcem co poprzednio
       przyznajemy +2, palcem obok +1.
    3. Zmiana rąk. Za literę wpisaną tą samą ręką co poprzednio: +1.
    4. Długość palca. Za literę wpisaną palcem serdecznym +1, małym palcem +2.

"""


if __name__ == '__main__':
    print __doc__

