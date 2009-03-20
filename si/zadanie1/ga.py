#!/usr/bin/env python
# -*- coding: UTF-8 -*-

import sys
import getopt
import random


usage = u"""\
Prosty algorytm genetyczny przeznaczony do poszukiwania drogi w labiryncie.
Użycie: python ga.py [opcje] PLIK_Z_LABIRYNTEM [LICZBA_EPOK]
Opcje:
        -t k  Selekcja turniejowa o rozmiarze turnieju k.
        -s m  Ustaw rozmiar populacji na m osobników.\
"""

m = 150 # rozmiar populacji
l = 70 # rozmiar chromosomu
p_c = .7 # prawdopodobieństwo krzyżowania
p_m = .7 # prawdopodobieństwo mutacji


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

    def phenotype(self):
        u"""Zwróć listę ruchów reprezentowanych przez genotyp osobnika.

        Dwa geny to jeden ruch. Możliwe ruchy: 'left', 'right', 'up', 'down'.

        """
        phenotype = []
        for i in range(0, len(self.genotype)-1, 2):
            if self.genotype[i] == 0 and self.genotype[i+1] == 0:
                phenotype.append('left')
            if self.genotype[i] == 0 and self.genotype[i+1] == 1:
                phenotype.append('right')
            if self.genotype[i] == 1 and self.genotype[i+1] == 0:
                phenotype.append('up')
            if self.genotype[i] == 1 and self.genotype[i+1] == 1:
                phenotype.append('down')
        return phenotype

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
                    row.append(0) # wolne pole
                elif c == 'S':
                    row.append(2)
                    self.start_pos = Coords(x, y) # pozycja początkowa
                elif c == 'E':
                    row.append(4)
                    self.end_pos = Coords(x, y) # pozycja końcowa
                elif c != '\n':
                    row.append(1) # zajęte pole
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
                if square == 3:
                    row_str += '@' # rozwiązanie
                if square == 4:
                    row_str += 'E'
            print row_str

    def move(self, pos, direction):
        u"""Zwróć naszą pozycję po wykonaniu podanego ruchu w labiryncie.

        pos - nasza aktualna pozycja
        direction - kierunek w jakim chcemy iść ('left', 'right', 'up' lub
        'down')
        Jeśli droga jest zablokowana, zwróć aktualną pozycję.

        """
        if direction == 'left':
            if pos.x > 0 and self.squares[pos.y][pos.x - 1] != 1:
                return Coords(pos.x - 1, pos.y)
        elif direction == 'right':
            if pos.x < self.width - 1 and self.squares[pos.y][pos.x + 1] != 1:
                return Coords(pos.x + 1, pos.y)
        elif direction == 'up':
            if pos.y > 0 and self.squares[pos.y - 1][pos.x] != 1:
                return Coords(pos.x, pos.y - 1)
        elif direction == 'down':
            if pos.y < self.height - 1 and self.squares[pos.y + 1][pos.x] != 1:
                return Coords(pos.x, pos.y + 1)
        return pos

    def mark_solution(self, specimen):
        u"""Zaznacz w labiryncie drogę jaką przejdzie dany osobnik."""
        position = self.start_pos
        for move in specimen.phenotype():
            position = maze.move(position, move)
            if position == maze.end_pos:
                break
            self.squares[position.y][position.x] = 3


class Coords:
    u"""Przechowuje współrzędne (x, y)."""
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __eq__(self, other):
        return self.x == other.x and self.y == other.y

    def __ne__(self, other):
        return self.x != other.x or self.y != other.y

    def __str__(self):
        return '(' + str(self.x) + ', ' + str(self.y) + ')'


def fitness(specimen):
    u"""Zwróć ocenę przystosowania danego osobnika w labiryncie."""
    # pozycja w jakiej znajdzie się osobnik po wykonaniu ruchów z genotypu
    position = maze.start_pos
    for move in specimen.phenotype():
        position = maze.move(position, move)
        if position == maze.end_pos:
            break
    manhattan = lambda pos1, pos2: abs(pos1.x - pos2.x) + abs(pos1.y - pos2.y)
    return 1.0 / (manhattan(position, maze.end_pos) + 1)


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
    p = list(population)
    candidates = []
    for i in range(k):
        specimen = random.choice(p)
        p.remove(specimen)
        candidates.append(specimen)
    del p
    return max(candidates)


def epoch(pop_size, g_len, p_c, p_m, target_fitness, selection, select_arg):
    u"""Jeden przebieg algorytmu genetycznego.

    pop_size - rozmiar populacji
    g_len - długość genotypu osobnika
    p_c - prawdopodobieństwo krzyżowania
    p_m - prawdopodobieństwo mutacji
    target_fitness - wartość przystosowania jaką chcemy osiągnąć
    selection - funkcja selekcji, do wybierania rodziców z populacji
    select_arg - argument przekazany do funkcji selekcji
    Zwraca listę najlepszych osobników w kolejnych populacjach.

    """
    population = [] # lista osobników (instancji klasy Specimen)
    best = [] # najlepsze osobniki w kolejnych populacjach
    # tworzymy początkową populację złożoną z osobników o losowym genotypie
    for i in range(pop_size):
        population.append(Specimen(fitness, genotype_len=g_len))
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
    try:
        options, args = getopt.getopt(sys.argv[1:], 's:t:', ['help'])
        if len(args) > 0:
            maze = Maze(args[0]) # wczytujemy labirynt z pliku
            selection = select_proportional # domyślna funkcja selekcji
            select_arg = None # argument przekazywany funkcji selekcji
            for option, argument in options:
                if option == '-t':
                    selection = select_tournament
                    select_arg = int(argument)
                elif option == '-s':
                    m = int(argument)

            if len(args) > 1 and int(args[1]) > 1: # wiele epok
                epoch_count = int(args[1])
                iterations = [] # liczba iteracji w kolejnych epokach
                print 'Liczba iteracji (pokoleń) dla kolejnych epok:'
                for i in range(epoch_count):
                    results = epoch(m, l, p_c, p_m, 1.0, selection, select_arg)
                    iterations.append(len(results))
                    print '%d\t%d' % (i+1, iterations[-1])
                print 'Uśredniona liczba iteracji:'
                average = float(sum(iterations)) / len(iterations)
                print '%.1f' % (average)

            else: # jedna epoka
                results = epoch(m, l, p_c, p_m, 1.0, selection, select_arg)

                print 'Najlepsze przystosowanie w kolejnych populacjach:'
                for i, best_in_population in enumerate(results):
                    print '%d\t%.2f' % (i+1, best_in_population.fitness)
                print 'Rozwiązanie:'
                maze.mark_solution(results[-1])
                maze.print_maze()

        else:
            print usage
    except IOError:
        print u'Błąd: nie można odczytać pliku', args[0]
        sys.exit(1)
    except ValueError:
        print u'Błąd: należy podać liczbę całkowitą'
        sys.exit(2)
    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)

