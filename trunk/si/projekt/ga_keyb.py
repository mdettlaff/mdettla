#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Szukanie optymalnego układu klawiatury za pomocą algorytmu genetycznego.

Stosujemy algorytm genetyczny z selekcją turniejową lub proporcjonalną.
Genotypem pojedynczego osobnika będzie ciąg znaków reprezentujący układ
klawiszy na klawiaturze. Przykładowo, dla układu QWERTY wygląda on tak:
"qwertyuiopasdfghjkl;zxcvbnm,.?".
Każdy znak może oczywiście wystąpić tylko raz.

Mutację definiujemy jako zamianę dwóch losowo wybranych znaków miejscami.
Niech X oznacza zmienną losową reprezentującą ilość mutacji dla osobnika.
Do jej obliczenia użyjemy wzoru:

    X = int(math.floor(1 / random.uniform(.1, 1)))

W przybliżeniu: P(X=1) = 55%, P(X=2) = 18%, P(X=3) =  9%, ..., P(X=9) = 1%.

Stosujemy krzyżowanie wielopunktowe.

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

__docformat__ = 'restructuredtext'

import random


m = 200
u"""Rozmiar populacji."""
p_c = .7
u"""Prawdopodobieństwo krzyżowania."""
p_m = .7
u"""Prawdopodobieństwo mutacji."""


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji.

    Osobnikiem jest układ klawiatury, reprezentowany przez ciąg znaków.

    """
    def __init__(self, fit_func, parents=None, p_c=.7, p_m=.7):
        u"""Utwórz nowego osobnika (losowo lub poprzez krzyżowanie).

        :Parameters:
            - `fit_func`: funkcja obliczająca przystosowanie osobnika
            - `parents`: rodzice osobnika, z których skrzyżowania powstanie;
              jeśli nie podano rodziców, tworzony jest osobnik z losowym
              genotypem
            - `p_c`: prawdopodobieństwo krzyżowania
            - `p_m`: prawdopodobieństwo mutacji

        """
        self.genotype = []
        if parents:
            self.__new_descendant(parents, p_c, p_m)
        else:
            self.__new_random_instance()
        self.fitness = fit_func(self)

    def __new_random_instance(self):
        u"""Utwórz nowego osobnika z losowo utworzonym genotypem."""
        keys = 'qwertyuiopasdfghjkl;zxcvbnm,.?'
        for key in keys:
            self.genotype.append(key)
        random.shuffle(self.genotype)

    def __new_descendant(self, parents, p_c):
        u"""Utwórz osobnika będącego potomkiem podanych rodziców."""
        self.genotype = list(random.choice(parents).genotype)
        # TODO
        # krzyżowanie
        if (random.random() < p_c):
            # przeprowadzamy krzyżowanie wielopunktowe
            pass
        # mutacja
        if (random.random() < p_m):
            pass

    def __eq__(self, other):
        return self.genotype == other.genotype

    def __ne__(self, other):
        return self.genotype != other.genotype

    def __cmp__(self, other):
        return cmp(self.fitness, other.fitness)

    def __str__(self):
        # TODO
        pass

    def phenotype(self):
        u"""Zwróć słownik z rzędami klawiszy: 'top', 'home', 'bottom'."""
        # TODO
        return None


def fitness(specimen):
    u"""Zwróć ocenę przystosowania danego osobnika."""
    # TODO
    return None


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

    Losuj bez powtórzeń k osobników i zwróć najlepiej przystosowanego.

    """
    return max(random.select(population, k))


def epoch(population_size, p_c, p_m, target_fitness, selection, select_arg):
    u"""Jeden przebieg algorytmu genetycznego.

    :Parameters:
        - `population_size`: rozmiar populacji
        - `p_c`: prawdopodobieństwo krzyżowania
        - `p_m`: prawdopodobieństwo mutacji
        - `target_fitness`: wartość przystosowania jaką chcemy osiągnąć
        - `selection`: funkcja selekcji, do wybierania rodziców z populacji
        - `select_arg`: argument przekazany do funkcji selekcji

    :Return:
        - Lista najlepiej przystosowanych osobników w kolejnych populacjach.

    """
    population = [] # lista osobników (instancji klasy Specimen)
    best = [] # najlepsze osobniki w kolejnych populacjach
    # tworzymy początkową populację złożoną z osobników o losowym genotypie
    for i in range(population_size):
        population.append(Specimen(fitness))
    best.append(max(population))
    while best[-1].fitness < target_fitness:
        new_population = []
        for i in range(len(population)):
            parent1 = selection(population, select_arg)
            parent2 = selection(population, select_arg)
            offspring = Specimen(fitness, (parent1, parent2), p_c, p_m)
            new_population.append(offspring)
        population = new_population
        best.append(max(population))
    return best


if __name__ == '__main__':
    print __doc__

