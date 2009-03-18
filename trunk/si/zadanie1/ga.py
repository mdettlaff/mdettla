#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
import random


usage = u"""\
Prosty algorytm genetyczny przeznaczony do poszukiwania drogi w labiryncie.
Użycie: python ga.py PLIK_Z_LABIRYNTEM\
"""

m = 15 # rozmiar populacji
l = 8 # rozmiar chromosomu
p_c = .7 # prawdopodobieństwo krzyżowania
p_m = .7 # prawdopodobieństwo mutacji

old_population = [] # lista osobników (instancji klasy Specimen)
new_population = [] # lista osobników (instancji klasy Specimen)
best = [] # najlepsze osobniki w kolejnych populacjach


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji."""

    def __init__(self, fit_func, parents=None, p_c=.7, p_m=0, genotype_len=10):
        u"""Utwórz nowego osobnika (losowo lub poprzez krzyżowanie).
        
        fit_func - funkcja obliczająca przystosowanie osobnika
        parents - rodzice osobnika, z których skrzyżowania powstanie; jeśli nie
        podano rodziców, tworzony jest osobnik z losowym genotypem
        p_c - prawdopodobieństwo krzyżowania
        p_m - prawdopodobieństwo mutacji
        genotype_len - długość genotypu (jeśli tworzymy losowego osobnika)
        
        """
        self.genotype = []
        if parents:
            self.__new_descendant(list(parents), p_c, p_m)
        else:
            self.__new_random_instance(genotype_len)
        self.fitness = fit_func(self)

    def __new_random_instance(self, genotype_len):
        u"""Utwórz nowego osobnika z losowo utworzonym genotypem."""
        for i in range(genotype_len):
            self.genotype.append(random.randint(0, 1))

    def __new_descendant(self, parents, p_c, p_m):
        u"""Utwórz osobnika będącego potomkiem podanych rodziców."""
        random.shuffle(parents)
        self.genotype = parents[0].genotype
        # krzyzowanie
        if (random.random() < p_c):
            # przeprowadzamy krzyżowanie jednopunktowe
            cut_index = random.randint(1, len(self.genotype)-1)
            self.genotype = parents[0].genotype[:cut_index] + \
                    parents[1].genotype[cut_index:]
        # mutacja
        if (random.random() < p_m):
            mut_index = random.randint(0, len(self.genotype)-1)
            self.genotype[mut_index] = 1 - self.genotype[mut_index]

    def __cmp__(self, other):
        return cmp(self.fitness, other.fitness)

    def __str__(self):
        genotype_str = ''
        for gene in self.genotype:
            genotype_str += str(gene)
        return genotype_str


class Maze:
    u"""Labirynt ze zdefiniowanym wejściem oraz wyjściem."""
    def __init__(self, filename):
        u"""Utwórz nowy labirynt na podstawie danych z pliku."""
        self.squares = [] # pola labiryntu o współrzędnych [y][x]

        f = open(filename)
        for y, line in enumerate(f.readlines()):
            row = []
            for x, c in enumerate(line):
                if c == ' ':
                    row.append(0)
                elif c == 'S':
                    row.append(2)
                    self.start_pos = Coords(x, y) # pozycja początkowa
                elif c == 'E':
                    row.append(4)
                    self.end_pos = Coords(x, y) # pozycja końcowa
                elif c != '\n':
                    row.append(1)
            self.squares.append(row)
        self.width = len(self.squares[0])
        self.height = len(self.squares)

    def print_maze(self):
        for row in self.squares:
            row_str = ''
            for square in row:
                if square == 0:
                    row_str += ' '
                if square == 1:
                    row_str += '#'
                if square == 2:
                    row_str += 'S'
                if square == 4:
                    row_str += 'E'
            print row_str

    def move(self, pos, direction):
        u"""Zwróć naszą pozycję po wykonaniu podanego ruchu w labiryncie.
        
        pos - nasza aktualna pozycja
        direction - kierunek w jakim chcemy przejść ('left', 'right', 'up' lub
        'down')
        Jeśli droga jest zablokowana, zwróć aktualną pozycję.
        
        """
        if direction == 'left':
            if pos.x > 0 and self.squares[pos.y][pos.x - 1] != 1:
                return Coords(pos.x - 1, pos.y)
            else:
                return pos
        elif direction == 'right':
            if pos.x < self.width - 1 and self.squares[pos.y][pos.x + 1] != 1:
                return Coords(pos.x + 1, pos.y)
            else:
                return pos
        elif direction == 'up':
            if pos.y > 0 and self.squares[pos.y - 1][pos.x] != 1:
                return Coords(pos.x, pos.y - 1)
            else:
                return pos
        elif direction == 'down':
            if pos.y < self.height - 1 and self.squares[pos.y + 1][pos.x] != 1:
                return Coords(pos.x, pos.y + 1)
            else:
                return pos


class Coords:
    u"""Przechowuje współrzędne (x, y)."""
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __str__(self):
        return '(' + str(self.x) + ', ' + str(self.y) + ')'


def fitness(specimen):
    u"""Zwróć ocenę przystosowania danego osobnika w danym labiryncie."""
    # pozycja w jakiej znajdzie się osobnik po wykonaniu ruchów z genotypu
    pos = maze.start_pos
    for i in range(0, len(specimen.genotype)-1, 2):
        if specimen.genotype[i] == 0 and specimen.genotype[i+1] == 0:
            pos = maze.move(pos, 'left')
        if specimen.genotype[i] == 0 and specimen.genotype[i+1] == 1:
            pos = maze.move(pos, 'right')
        if specimen.genotype[i] == 1 and specimen.genotype[i+1] == 0:
            pos = maze.move(pos, 'up')
        if specimen.genotype[i] == 1 and specimen.genotype[i+1] == 1:
            pos = maze.move(pos, 'down')
        if pos == maze.end_pos:
            break
    manhattan = lambda pos1, pos2: abs(pos1.x - pos2.x) + abs(pos1.y - pos2.y)
    return 1.0 / (manhattan(pos, maze.end_pos) + 1)


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            maze = Maze(sys.argv[1])

            # tworzymy początkową populację
            for i in range(m):
                old_population.append(Specimen(fitness, genotype_len=l))

            # informacje do debugowania
            population = old_population
            maze.print_maze()
            for specimen in population:
                print specimen
            print u'Potomek dwóch ostatnich:',
            s = Specimen(fitness, (population[-1], population[-2]), p_c, p_m)
            print s
            print 'Przystosowanie:', s.fitness
            best.append(max(population))
            print 'Najlepszy osobnik:', best[0]
            print 'Przystosowanie:', best[0].fitness
        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', sys.argv[1]

