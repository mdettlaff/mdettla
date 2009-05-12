#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Przykład zastosowania algorytmu genetycznego.

Rozstawienie jak najmniejszej liczby hetmanów, żeby zaszachowane były
wszystkie pola.

"""

import math
import random
import sys


BOARD_SIZE = 8
u"""Rozmiar planszy."""
SELECTION = 't'
u"""Metoda selekcji: 't' - turniejowa, 'p' - proporcjonalna."""
ITERATIONS = 100
u"""Liczba iteracji."""
m = 150
u"""Rozmiar populacji."""
l = BOARD_SIZE ** 2
u"""Rozmiar chromosomu."""
p_c = .7
u"""Prawdopodobieństwo krzyżowania."""
p_m = .7
u"""Prawdopodobieństwo mutacji."""


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji."""
    def __init__(self, fit_func, parents=None, p_c=.7, p_m=0, genotype_len=10):
        u"""Utwórz nowego osobnika (losowo lub poprzez krzyżowanie).

        :Parameters:
            - `fit_func`: Funkcja obliczająca przystosowanie osobnika.
            - `parents`: Rodzice osobnika, z których skrzyżowania powstanie.
              Jeśli nie podano rodziców, tworzony jest osobnik z losowym
              genotypem.
            - `p_c`: Prawdopodobieństwo krzyżowania.
            - `p_m`: Prawdopodobieństwo mutacji.
            - `genotype_len`: Długość genotypu (jeśli tworzymy losowego
              osobnika).

        """
        self.genotype = []
        if parents:
            self.__new_descendant(list(parents), p_c, p_m)
        else:
            self.__new_random_instance(genotype_len)
        self.fitness = fit_func(self.phenotype)

    def __new_random_instance(self, genotype_len):
        u"""Utwórz nowego osobnika z losowo utworzonym genotypem."""
        for i in range(genotype_len):
            self.genotype.append(random.randint(0, 1))

    def __new_descendant(self, parents, p_c, p_m):
        u"""Utwórz osobnika będącego potomkiem podanych rodziców."""
        random.shuffle(parents)
        self.genotype = list(parents[0].genotype)
        # krzyżowanie
        if (random.random() < p_c):
            # przeprowadzamy krzyżowanie jednopunktowe
            cut_index = random.randint(1, len(self.genotype)-1)
            self.genotype = parents[0].genotype[:cut_index] + \
                    parents[1].genotype[cut_index:]
        # mutacja
        if (random.random() < p_m):
            mut_index = random.randint(0, len(self.genotype)-1)
            self.genotype[mut_index] = 1 - self.genotype[mut_index]

    @property
    def phenotype(self):
        return Board(self.genotype)

    def __eq__(self, other):
        return self.genotype == other.genotype

    def __ne__(self, other):
        return self.genotype != other.genotype

    def __cmp__(self, other):
        return cmp(self.fitness, other.fitness)

    def __str__(self):
        genotype_str = ''
        for gene in self.genotype:
            genotype_str += str(gene)
        return genotype_str


class Board:
    def __init__(self, genotype):
        N = int(math.sqrt(len(genotype)))
        self.squares = []
        for i, gene in enumerate(genotype):
            if i % N == 0:
                self.squares.append([])
            self.squares[-1].append(gene)

    @property
    def queens_count(self):
        u"""Zwróc ilość królowch na planszy."""
        queens_count = 0
        for row in self.squares:
            for square in row:
                if square == 1:
                    queens_count += 1
        return queens_count

    @property
    def free_squares_count(self):
        u"""Zwróć ilość niezaszachowanych pól na planszy."""
        free_squares_count = 0
        for j, row in enumerate(self.squares):
            for i, square in enumerate(row):
                if self.is_square_free(i, j):
                    free_squares_count += 1
        return free_squares_count

    def is_square_free(self, x, y):
        u"""Sprawdź czy podane pole jest niezaszachowane."""
        for j, row in enumerate(self.squares):
            for i in range(len(row)):
                if self.squares[j][i] == 1 and \
                        (x == i or y == j or abs(x - i) == abs(y - j)):
                    return False
        return True

    def __str__(self):
        s = ''
        line = '+'
        for j in range(len(self.squares)):
            line += '---+'
        for row in self.squares:
            s += line + '\n'
            for x in range(len(row)):
                s += '| '
                if row[x] == 1:
                    s += 'X '
                else:
                    s += '  '
            s += '|\n'
        s += line
        return s


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

    Losuj bez powtórzeń `k` osobników z populacji `population` i zwróć
    najlepiej przystosowanego.

    """
    return max(random.sample(population, k))


def epoch(pop_size, g_len, p_c, p_m, iter, selection, select_arg):
    u"""Jeden przebieg algorytmu genetycznego.

    :Parameters:
        - `pop_size`: Rozmiar populacji.
        - `g_len`: Długość genotypu osobnika.
        - `p_c`: Prawdopodobieństwo krzyżowania.
        - `p_m`: Prawdopodobieństwo mutacji.
        - `iter`: Ilość iteracji (pokoleń) algorytmu.
        - `selection`: Funkcja selekcji, do wybierania rodziców z populacji.
        - `select_arg`: Argument przekazany do funkcji selekcji.

    :Return:
        - Lista z najlepszymi osobnikami w kolejnych populacjach.

    """
    population = [] # lista osobników (instancji klasy Specimen)
    # tworzymy początkową populację złożoną z osobników o losowym genotypie
    for i in range(pop_size):
        population.append(Specimen(fitness, genotype_len=g_len))
    yield max(population)
    for j in range(iter - 1):
        new_population = []
        for i in range(len(population)):
            parent1 = selection(population, select_arg)
            parent2 = selection(population, select_arg)
            offspring = Specimen(fitness, (parent1, parent2), p_c, p_m)
            new_population.append(offspring)
        population = new_population
        yield max(population)


def fitness(board):
    u"""Zwróć ocenę przystosowania fenotypu osobnika (ustawienia królowych)."""
    return 1 * (-1) * board.queens_count + \
            1000 * (-1) * board.free_squares_count


def main(argv):
    if SELECTION == 't':
        selection = select_tournament
        select_arg = 4 # rozmiar turnieju
    elif SELECTION == 'p':
        selection = select_proportional
        select_arg = None
    else:
        print u'Nieznana funkcja selekcji.'
        sys.exit(1)

    print u'Najlepsze przystosowanie w kolejnych populacjach -',
    if selection == select_proportional:
        print u'selekcja proporcjonalna:'
    else:
        print u'selekcja turniejowa:'
    results = []
    for i, best in enumerate(epoch(m, l, p_c, p_m, ITERATIONS,
        selection, select_arg)):
        results.append(best)
        print '%d\t%.2f' % (i+1, best.fitness)
    print u'Najlepszy wynik:'
    print max(results).phenotype
    print u'Ilość niezaszachowanych pól:', \
            max(results).phenotype.free_squares_count
    print u'Ilość królowych:', \
            max(results).phenotype.queens_count


if __name__ == '__main__':
    main(sys.argv)

