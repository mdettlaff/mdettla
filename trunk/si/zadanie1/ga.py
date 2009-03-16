#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
import random


usage = u"""\
Prosty algorytm genetyczny przeznaczony do poszukiwania drogi w labiryncie.
Użycie: python """ + sys.argv[0] + """ PLIK_Z_LABIRYNTEM\
"""

m = 15 # rozmiar populacji
l = 7 # rozmiar chromosomu
p_c = .7 # prawdopodobieństwo krzyżowania
p_m = .7 # prawdopodobieństwo mutacji

maze = [] # labirynt; maze[y][x]
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
            self.genotype[mut_index] = int(not
                    self.genotype[mut_index])

    def __str__(self):
        genotype_str = ''
        for gene in self.genotype:
            genotype_str += str(gene)
        return genotype_str


def read_input(filename, maze):
    u"""Wczytaj labirynt z pliku."""
    f = open(filename)
    for line in f.readlines():
        row = []
        for c in line:
            if c == ' ':
                row.append(0)
            elif c == 'S':
                row.append(2)
            elif c == 'E':
                row.append(4)
            else:
                row.append(1)
        maze.append(row)


def print_maze(maze):
    for row in maze:
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


if __name__ == '__main__':
    try:
        if len(sys.argv) > 1:
            read_input(sys.argv[1], maze)

            # tworzymy początkową populację
            for i in range(m):
                population.append(Specimen(genotype_len=l))

            print_maze(maze)
            for specimen in population:
                print specimen
            print 'Potomek:'
            print Specimen((population[0], population[1]), p_c, p_m)
        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', sys.argv[1]

