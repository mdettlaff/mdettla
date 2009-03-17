#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
import random


usage = u"""\
Prosty algorytm genetyczny przeznaczony do poszukiwania drogi w labiryncie.
Użycie: python """ + sys.argv[0] + """ PLIK_Z_LABIRYNTEM\
"""

m = 15 # rozmiar populacji
l = 8 # rozmiar chromosomu
p_c = .7 # prawdopodobieństwo krzyżowania
p_m = .7 # prawdopodobieństwo mutacji

population = [] # lista osobników (instancji klasy Specimen)


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji."""

    def __init__(self, parents=None, p_c=0, p_m=0, genotype_len=0):
        u"""Utwórz nowego osobnika (losowo lub poprzez krzyżowanie)."""
        self.genotype = []
        if parents:
            self.__new_descendant(list(parents), p_c, p_m)
        else:
            self.__new_random_instance(genotype_len)

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
            print u'krzyżowanie na pozycji:', cut_index
            self.genotype = parents[0].genotype[:cut_index] + \
                    parents[1].genotype[cut_index:]
        # mutacja
        if (random.random() < p_m):
            mut_index = random.randint(0, len(self.genotype)-1)
            print 'mutacja na pozycji:', mut_index
            self.genotype[mut_index] = 1 - self.genotype[mut_index]

    def __str__(self):
        genotype_str = ''
        for gene in self.genotype:
            genotype_str += str(gene)
        return genotype_str


class Maze:
    u"""Labirynt ze zdefiniowanym wejściem oraz wyjściem."""
    def __init__(self, filename):
        u"""Utwórz nowy labirynt na podstawie danych z pliku."""
        self.squares = [] # zawartości pól labiryntu na pozycjach [y][x]
        self.start_pos = None # tupla z pozycją startową w labyryncie: (y, x)
        self.end_pos = None # tupla z pozycją końcową w labyryncie: (y, x)

        f = open(filename)
        for y, line in enumerate(f.readlines()):
            row = []
            for x, c in enumerate(line):
                if c == ' ':
                    row.append(0)
                elif c == 'S':
                    row.append(2)
                    self.start_pos = (y, x)
                elif c == 'E':
                    row.append(4)
                    self.end_pos = (y, x)
                elif c != '\n':
                    row.append(1)
            self.squares.append(row)

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


def fitness(specimen, maze):
    u"""Zwróć ocenę przystosowania danego osobnika w danym labiryncie."""
    for i in range(0, len(specimen.genotype)-1, 2):
        if specimen.genotype[i] == 0 and specimen.genotype[i+1] == 0:
            print 'left'
        if specimen.genotype[i] == 0 and specimen.genotype[i+1] == 1:
            print 'right'
        if specimen.genotype[i] == 1 and specimen.genotype[i+1] == 0:
            print 'up'
        if specimen.genotype[i] == 1 and specimen.genotype[i+1] == 1:
            print 'down'
    return .0


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            maze = Maze(sys.argv[1])

            # tworzymy początkową populację
            for i in range(m):
                population.append(Specimen(genotype_len=l))

            # informacje do debugowania
            maze.print_maze()
            print maze.start_pos
            print maze.end_pos
            for specimen in population:
                print specimen
            print 'Potomek:'
            s = Specimen((population[0], population[1]), p_c, p_m)
            print s
            print 'Przystosowanie:', fitness(s, maze)
        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', sys.argv[1]

