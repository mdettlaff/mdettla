#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Szukanie optymalnego układu klawiatury za pomocą algorytmu genetycznego.

Stosujemy algorytm genetyczny z selekcją turniejową lub proporcjonalną.
Genotypem pojedynczego osobnika będzie ciąg znaków reprezentujący układ
klawiszy na klawiaturze. Przykładowo, dla układu QWERTY wygląda on tak:
"qwertyuiopasdfghjkl;zxcvbnm,.?".
Każdy znak może oczywiście wystąpić tylko raz.

Mutację definiujemy jako zamianę dwóch losowo wybranych znaków miejscami.
Ilość mutacji dla osobnika (oznaczmy ją jako zmienną losową X), obliczamy
w następujący sposób:

    X = int(math.floor(1 / random.uniform(.1, 1)))

W przybliżeniu: P(X=1) = 55%, P(X=2) = 18%, P(X=3) =  9%, ..., P(X=9) = 1%.

Ze względu na to, że genotyp musi być permutacją znaków, krzyżownie w ścisłym
znaczeniu nie jest możliwe. Zastosujemy jednak operację podobną do krzyżowania
jednostajnego, naprawiając na bieżąco powtórzenia znaków - wywoła to
z konieczności kilka mutacji podczas każdego krzyżowania.

Funkcja oceny (przystosowania) obliczana jest za pomocą wzoru

    c1*w1 + c2*w2 + ... + cN*wN,

gdzie c1,...,cN oznaczają cechy układu klawiatury, a w1,...,wN to ich wagi,
decydujące o wpływie danej cechy na ogólną ocenę. Im większa wartość funkcji
oceny, tym lepsze przystosowanie osobnika. Wartości liczbowe są przypisywane
cechom na podstawie analizy reprezentatywnego zbioru tekstów.

Pod uwagę brane są następujące cechy:

1. Rząd klawiszy. Za literę w środkowym rzędzie wartość zwiększana jest o 1, \
w górnym o 0.5, a w dolnym 0. Ruch palca wskazującego do środka: +0.5.
2. Bliskość klawiszy. Za literę wpisaną tym samym palcem co poprzednio \
przyznajemy 0, palcem obok +0.5. W pozostałych przypadkach +1.
3. Zmiana rąk. Za literę wpisaną inną ręką niż poprzednio +1.
4. Długość palca. Za literę wpisaną małym palcem 0, serdecznym +0.5, reszta +1.

"""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import getopt
import math
import random
import sys


usage = u"""\
Użycie: python ga_keyb.py [opcje] PLIK...
Plik tekstowy PLIK zostanie użyty do oceny przystosowania.
Opcje:
    -c P    Ustaw prawdopodobieństwo krzyżowania na P (0 <= P <= 1).
    -m P    Ustaw prawdopodobieństwo mutacji na P (0 <= P <= 1).
    -s N    Ustaw rozmiar populacji na N osobników.
    --help  Wyświetl treść pomocy i zakończ.\
"""

DEFAULT_POPULATION_SIZE = 200
u"""Domyślny rozmiar populacji."""
DEFAULT_P_C = .7
u"""Domyślne prawdopodobieństwo krzyżowania."""
DEFAULT_P_M = .7
u"""Domyślne prawdopodobieństwo mutacji."""


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji.

    Osobnikiem jest układ klawiatury, reprezentowany przez ciąg znaków.

    """
    def __init__(self, fit_func, fit_args, parents=None, p_c=.7, p_m=.7):
        u"""Utwórz nowego osobnika (losowo lub poprzez krzyżowanie).

        :Parameters:
            - `fit_func`: Funkcja obliczająca przystosowanie osobnika. Jako
              argumenty musi brać instancję osobnika oraz `fit_args`.
            - `fit_args`: Lista argumentów przekazywanych do `fit_func`.
            - `parents`: Rodzice osobnika, z których skrzyżowania powstanie.
              Jeśli nie podano rodziców, tworzony jest osobnik z losowym
              genotypem.
            - `p_c`: Prawdopodobieństwo krzyżowania.
            - `p_m`: Prawdopodobieństwo mutacji.

        """
        self.genotype = []
        if parents:
            self.__new_descendant(parents, p_c, p_m)
        else:
            self.__new_random_instance()
        self.fitness = fit_func(self, *fit_args)

    def __new_random_instance(self):
        u"""Utwórz nowego osobnika z losowo utworzonym genotypem."""
        chars = u'qwertyuiopasdfghjkl;zxcvbnm,.?'
        for c in chars:
            self.genotype.append(c)
        random.shuffle(self.genotype)

    def __new_descendant(self, parents, p_c, p_m):
        u"""Utwórz osobnika będącego potomkiem podanych rodziców."""
        self.genotype = list(parents[random.randint(0, 1)].genotype)
        # krzyżowanie jednostajne z naprawianiem powtórzeń
        if (random.random() < p_c):
            print u'krzyżowanie'
            # znaki, których jeszcze nie skopiowaliśmy do genotypu potomka
            unused = set(self.genotype)
            random_order = range(0, len(self.genotype))
            random.shuffle(random_order)
            for i in random_order:
                parent = random.randint(0, 1)
                if parents[parent].genotype[i] in unused:
                    key = parents[parent].genotype[i]
                    self.genotype[i] = key
                    unused.remove(key)
                elif parents[1 - parent].genotype[i] in unused:
                    key = parents[1 - parent].genotype[i]
                    self.genotype[i] = key
                    unused.remove(key)
                else: # bierzemy arbitralnie wybrany niepowtarzający się znak
                    self.genotype[i] = unused.pop()
        # mutacja
        if (random.random() < p_m):
            mut_count = int(math.floor(1 / random.uniform(.1, 1)))
            print u'mutacja X', mut_count
            # pozycje, na których wystąpią mutacje
            mp = random.sample(range(0, len(self.genotype)), mut_count * 2)
            for i in range(0, len(mp), 2):
                self.genotype[mp[i]], self.genotype[mp[i+1]] = \
                        self.genotype[mp[i+1]], self.genotype[mp[i]]

    def __eq__(self, other):
        return self.genotype == other.genotype

    def __ne__(self, other):
        return self.genotype != other.genotype

    def __lt__(self, other):
        return self.fitness < other.fitness

    def __le__(self, other):
        return self.fitness <= other.fitness

    def __gt__(self, other):
        return self.fitness > other.fitness

    def __ge__(self, other):
        return self.fitness >= other.fitness

    def __cmp__(self, other):
        return cmp(self.fitness, other.fitness)

    def __str__(self):
        string = ''
        for row in self.phenotype:
            for i, key in enumerate(row):
                string += key + ' '
                if i == 4:
                    string += ' '
            string += '\n'
        return string[:-1]

    @property
    def phenotype(self):
        u"""Lista z rzędami klawiszy: [górny, środkowy, dolny]."""
        return [self.genotype[:10], self.genotype[10:20], self.genotype[20:]]


def main():
    population_size = DEFAULT_POPULATION_SIZE
    p_c = DEFAULT_P_C # prawdopodobieństwo krzyżowania
    p_m = DEFAULT_P_M # prawdopodobieństwo mutacji
    corpus = u'' # tekst analizowany w celu oceny przystosowania

    pl_chars_to_latin = lambda string: \
            string.lower().replace(u'ą', 'a').replace(u'ć', 'c') \
            .replace(u'ę', 'e').replace(u'ł', 'l').replace(u'ń', 'n') \
            .replace(u'ó', 'o').replace(u'ś', 's').replace(u'ż', 'z') \
            .replace(u'ź', 'z')
    try:
        options, args = getopt.getopt(sys.argv[1:], 'hs:c:m:', ['help'])
        for option, argument in options:
            if option in ('-h', '--help'):
                print __doc__.split('\n')[0]
                print usage
                sys.exit()
            elif option == '-s':
                population_size = int(argument)
            elif option == '-c':
                p_c = float(argument)
            elif option == '-m':
                p_m = float(argument)

        if args:
            for filename in args:
                for line in open(filename).readlines():
                    corpus += line.decode('UTF-8')
            corpus = pl_chars_to_latin(corpus)
            print corpus

            p1 = Specimen(fitness, [corpus] + 4*[1])
            p2 = Specimen(fitness, [corpus] + 4*[1])
            offspring = Specimen(fitness, [corpus] + 4*[1], (p1, p2), p_c, p_m)
            print p1
            print p1.fitness, '\n'
            print p2
            print p2.fitness, '\n'
            print offspring
            print offspring.fitness
        else:
            print usage

    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)


def fitness(specimen, corpus, rows_weight=1, distance_weight=1,
        alteration_weight=1, fingers_weight=1):
    u"""Funkcja przystosowania.

    Przy obliczaniu funkcji przystosowania bierzemy pod uwagę następujące cechy
    układu klawiatury: rząd klawisza, odległość poprzedniego klawisza,
    alteracja rąk, palcowanie (dłuższe palce powinny wykonywać więcej pracy).
    O wpływie danej cechy na wynik decydują wagi (patrz parametry).

    :Parameters:
        - `specimen`: Układ klawiatury, którego przystosowanie obliczamy.
        - `corpus`: Tekst (najlepiej dość długi), na podstawie analizy którego
          obliczana jest wartość przystosowania układu klawiatury.
        - `rows_weight`: Waga cechy rząd klawisza.
        - `distance_weight`: Waga cechy odległość od poprzedniego klawisza.
        - `alteration_weight`: Waga cechy alteracja rąk.
        - `fingers_weight`: Waga cechy palcowanie.

    :Return:
        - Ocena danego układu klawiatury. Im wyższa, tym lepsza.

    """

    rows = 0 # cecha: rząd klawiszy
    distance = 0 # cecha: odległość poprzedniego klawisza
    alteration = 0 # cecha: zmiana rąk
    fingers = 0 # cecha: palcowanie (dłuższy palec powinien więcej pisać)
    prev_row, prev_column = None, None
    for c in corpus: # iterujemy kolejno po znakach w tekście
        if c not in specimen.genotype:
            prev_row = None
            prev_column = None
            continue
        # znajdujemy pozycję znaku na danym układzie klawiatury
        for i, r in enumerate(specimen.phenotype):
            if c in r:
                row = i
                column = r.index(c)
                break
        # obliczamy cechę: rząd klawiszy
        if row == 1 and (3 < column < 6): # 'gh' na QWERTY
            rows += .5
        elif row == 1: # środkowy (home row)
            rows += 1
        elif row == 0 and (3 < column < 6): # 'ty' na QWERTY
            rows += .2
        elif row == 0: # górny
            rows += .5
        elif row == 2 and column == 4: # 'b' na QWERTY
            rows += 0
        elif row == 2: # dolny
            rows += .1
        # obliczamy cechę: odległość poprzedniego klawisza
        if prev_row and prev_column:
            d = 1 # ile dodać do distance
            if column == prev_column:
                d -= .5
            elif abs(column - prev_column) == 1: # klawisz obok
                d -= .3
            elif abs(row - prev_row) == 2: # skok z góry na dół lub odwrotnie
                d -= .5
            distance += d
        # obliczamy cechę: alteracja rąk
        if (prev_row < 5 and row >= 5) or (prev_row >= 5 and row < 5):
            alteration += 1
        # obliczamy cechę: palcowanie
        if row == 0 or row == 9:
            fingers += 0 # mały palec
        if row == 1 or row == 8:
            fingers += .5 # palec serdeczny
        else:
            fingers += 1 # palec środkowy lub wskazujący
        # pamiętamy pozycję poprzedniego znaku w tekście
        prev_row = row
        prev_column = column
    return (rows * rows_weight) + (distance * distance_weight) + \
            (alteration * alteration_weight) + (fingers * fingers_weight)


def epoch(iterations, population_size, p_c, p_m, selection, select_args,
        fitness, fit_args):
    u"""Jeden przebieg algorytmu genetycznego.

    :Parameters:
        - `iterations`: Ilość iteracji (pokoleń) algorytmu.
        - `population_size`: Rozmiar populacji.
        - `p_c`: Prawdopodobieństwo krzyżowania.
        - `p_m`: Prawdopodobieństwo mutacji.
        - `selection`: Funkcja selekcji, do wybierania rodziców z populacji.
          Jako argumenty musi brać populację oraz `select_args`.
        - `select_args`: Argumenty przekazywane do funkcji `selection`.
        - `fitness`: Funkcja oceniająca przystosowanie osobników. Jako
          argumenty musi brać instancję osobnika oraz `fit_args`.
        - `fit_args`: Lista argumentów przekazywanych do funkcji `fitness`.

    :Return:
        - Lista najlepiej przystosowanych osobników w kolejnych populacjach.

    """
    population = [] # lista osobników (instancji klasy Specimen)
    best = [] # najlepsze osobniki w kolejnych populacjach
    # tworzymy początkową populację złożoną z osobników o losowym genotypie
    for i in range(population_size):
        population.append(Specimen(fitness, fit_args))
    best.append(max(population))
    for i in range(iterations):
        new_population = []
        for i in range(len(population)):
            parent1 = selection(population, *select_args)
            parent2 = selection(population, *select_args)
            parents = (parent1, parent2)
            offspring = Specimen(fitness, fit_args, parents, p_c, p_m)
            new_population.append(offspring)
        population = new_population
        best.append(max(population))
    return best


def select_proportional(population, *args):
    u"""Selekcja proporcjonalna.

    Wylosuj i zwróć osobnika z populacji z prawdopodobieństwem proporcjonalnym
    do jego przystosowania.

    """
    total_fitness = 0
    for specimen in population:
        total_fitness += specimen.fitness
    r = random.random() * total_fitness
    sum_fitness = 0
    for specimen in population:
        sum_fitness += specimen.fitness
        if sum_fitness > r:
            break
    return specimen


def select_tournament(population, k):
    u"""Selekcja turniejowa.

    Losuj bez powtórzeń `k` osobników z danej populacji i zwróć najlepiej
    przystosowanego.

    """
    return max(random.sample(population, k))


if __name__ == '__main__':
    main()

