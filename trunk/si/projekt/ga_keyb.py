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

import getopt
import math
import random
import sys


usage = u"""\
Użycie: python ga_keyb.py [opcje]
Opcje:
    -c p    Ustaw prawdopodobieństwo krzyżowania na p (0 <= p <= 1).
    -m p    Ustaw prawdopodobieństwo mutacji na p (0 <= p <= 1).
    -s m    Ustaw rozmiar populacji na m osobników.
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
        keys = 'qwertyuiopasdfghjkl;zxcvbnm,.?'
        for key in keys:
            self.genotype.append(key)
        random.shuffle(self.genotype)

    def __new_descendant(self, parents, p_c, p_m):
        u"""Utwórz osobnika będącego potomkiem podanych rodziców."""
        self.genotype = list(parents[random.randint(0, 1)].genotype)
        # krzyżowanie jednostajne z naprawianiem powtórzeń
        if (random.random() < p_c):
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
                    print 'arbitralny znak na pozycji', i
                    self.genotype[i] = unused.pop()
        # mutacja
        if (random.random() < p_m):
            mut_count = int(math.floor(1 / random.uniform(.1, 1)))
            # pozycje, na których wystąpią mutacje
            mp = random.sample(range(0, len(self.genotype)), mut_count * 2)
            print 'mutacje na pozycjach:', mp
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

        p1 = Specimen(fitness, ['asdf'])
        p2 = Specimen(fitness, ['asdf'])
        offspring = Specimen(fitness, ['asdf'], (p1, p2), p_c, p_m)
        print p1, '\n'
        print p2, '\n'
        print offspring

    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)


def fitness(specimen, corpus):
    u"""Funkcja przystosowania.

    :Parameters:
        - `specimen`: Układ klawiatury, którego przystosowanie obliczamy.
        - `corpus`: Tekst (najlepiej dość długi), na podstawie analizy którego
          obliczana jest wartość przystosowania układu klawiatury.

    :Return:
        - Ocena danego układu klawiatury. Im wyższa, tym lepsza.

    """
    # TODO
    return 0


def epoch(iterations, population_size, p_c, p_m, selection, select_args, \
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
    return max(random.select(population, k))


if __name__ == '__main__':
    main()

