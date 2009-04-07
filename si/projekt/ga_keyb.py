#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Szukanie optymalnego układu klawiatury za pomocą algorytmu genetycznego.

Stosujemy algorytm genetyczny z selekcją turniejową.
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

Funkcja oceny (przystosowania) układu klawiatury obliczana jest na podstawie
analizy reprezentatywnego zbioru tekstów. Do optymalizowanych zmiennych należą:

    - Rząd klawiszy. Najlepszy jest środkowy, najmniej optymalny dolny.
    - Użycie palców. Najwięcej pracy powinny wykonywać najdłuższe palce.
    - Użycie rąk. Prawa ręka powinna wykonywać więcej pracy niż lewa.
    - Zmiana rąk. Niekorzystne jest pisanie kolejnych liter tą samą ręką.
    - Zmiana palca. Należy unikać pisania kolejnych liter tym samym palcem.
    - Ruchy do środka. Sąsiednie klawisze powinny być pisane ruchem do środka.

Na początku wartość przystosowania wynosi 0, po czym za odstępstwa od
powyższych przyznawane są punkty karne. Zatem, im mniejsza wartość funkcji
oceny, tym lepsze przystosowanie osobnika.

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
    -c P    Prawdopodobieństwo krzyżowania (0 <= P <= 1).
    -e ENC  Kodowanie znaków w plikach wejściowych (domyślnie UTF-8).
    -i ITER Liczba ITERACJI (pokoleń) algorytmu genetycznego.
    -m P    Prawdopodobieństwo mutacji (0 <= P <= 1).
    -s ROZ  ROZMIAR (ilość osobników) populacji.
    -t K    Rozmiar turnieju K w selekcji turniejowej.
    -w N    Analizuj tylko N najczęściej występujących wyrazów.
    --help  Wyświetl treść pomocy i zakończ.\
"""

DEFAULT_ITERATIONS = 150
u"""Domyślna ilość iteracji (pokoleń) algorytmu genetycznego."""
DEFAULT_POPULATION_SIZE = 100
u"""Domyślny rozmiar populacji."""
DEFAULT_TOURNAMENT_SIZE = 4
u"""Domyślny rozmiar turnieju dla selekcji turniejowej."""
DEFAULT_P_C = .5
u"""Domyślne prawdopodobieństwo krzyżowania."""
DEFAULT_P_M = .7
u"""Domyślne prawdopodobieństwo mutacji."""
DEFAULT_WORDS = 200
u"""Domyślna maksymalna ilość najczęstszych wyrazów do analizowania."""
DEFAULT_ENCODING = 'UTF-8'
u"""Domyślne kodowanie znaków w plikach tekstowych do analizy."""


class Specimen:
    u"""Osobnik o określonym genotypie należący do populacji.

    Osobnikiem jest układ klawiatury, reprezentowany przez ciąg znaków.

    """
    def __init__(self, fit_func, fit_args, parents=None, p_c=.7, p_m=.7,
            clone=None):
        u"""Utwórz nowego osobnika (losowo, przez krzyżowanie lub klonowanie).

        :Parameters:
            - `fit_func`: Funkcja obliczająca przystosowanie osobnika. Jako
              argumenty musi brać instancję osobnika oraz `fit_args`.
            - `fit_args`: Lista argumentów przekazywanych do `fit_func`.
            - `parents`: Rodzice osobnika, z których skrzyżowania powstanie.
              Jeśli nie podano rodziców, tworzony jest osobnik z losowym
              genotypem.
            - `p_c`: Prawdopodobieństwo krzyżowania.
            - `p_m`: Prawdopodobieństwo mutacji.
            - `clone`: Genotyp, który zostanie przypisany nowemu osobnikowi.
              Klon powstanie, jeśli podano ten argument a nie podano rodziców.

        """
        self.genotype = []
        if parents:
            self.__new_descendant(parents, p_c, p_m)
        elif not clone:
            self.__new_random_instance()
        else:
            self.genotype = clone
        self.fitness = fit_func(self, *fit_args)

    def __new_random_instance(self):
        u"""Utwórz nowego osobnika z losowo utworzonym genotypem."""
        chars = u"qwertyuiopasdfghjkl;zxcvbnm,.'"
        for c in chars:
            self.genotype.append(c)
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
                    self.genotype[i] = unused.pop()
        # mutacja
        if (random.random() < p_m):
            mut_count = int(math.floor(1 / random.uniform(.1, 1)))
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


class Corpus:
    u"""Dane, których użyjemy do obliczenia przystosowania.

    Przechowuje słowa oraz przypisane im częstotliwości występowania,
    utworzone na podstawie analizy zbioru tekstów.

    """
    def __init__(self, filenames, encoding, size):
        u"""Utwórz nowe dane do analizy.

        :Parameters:
            - `filenames`: Nazwy plików tekstowych, które będziemy analizować.
            - `encoding`: Kodowanie znaków w plikach do analizy.
            - `size`: Pamiętamy tylko `size` najczęściej występujących słów.

        """
        self.frequencies = {} # słowa i ich częstotliwości
        pl2latin = lambda string: \
                string.lower().replace(u'ą', 'a').replace(u'ć', 'c') \
                .replace(u'ę', 'e').replace(u'ł', 'l').replace(u'ń', 'n') \
                .replace(u'ó', 'o').replace(u'ś', 's').replace(u'ż', 'z') \
                .replace(u'ź', 'z')
        for filename in filenames:
            for line in open(filename).readlines():
                for word in pl2latin(line.decode(encoding)).split():
                    if word in self.frequencies:
                        self.frequencies[word] += 1
                    else:
                        self.frequencies[word] = 1
        self.frequencies = dict(sorted(self.frequencies.iteritems(),
            key=lambda x: x[1], reverse=True)[:size])

    def __str__(self):
        s = ''
        for i, (word, freq) in enumerate(sorted(self.frequencies.iteritems(),
                key=lambda x: x[1], reverse=True)):
            s += str(freq) + '\t' + word + '\n'
        return s[:-1]


def main(argv):
    iterations = DEFAULT_ITERATIONS
    population_size = DEFAULT_POPULATION_SIZE
    tournament_size = DEFAULT_TOURNAMENT_SIZE
    p_c = DEFAULT_P_C # prawdopodobieństwo krzyżowania
    p_m = DEFAULT_P_M # prawdopodobieństwo mutacji
    words = DEFAULT_WORDS
    encoding = DEFAULT_ENCODING

    def print_stats(specimen, corpus):
        print specimen
        print u'przystosowanie:\t\t', specimen.fitness
        stats = statistics(specimen, corpus).split('\n')
        print u'rzędy klawiszy:\t\t', stats[0]
        print u'palce lewej ręki:\t', stats[1][3:]
        print u'palce prawej ręki:\t', stats[2][3:]
        print u'użycie rąk:\t\t', stats[3]
        print u'alternacja rąk:\t\t', stats[4]
        print u'zmiana palca:\t\t', stats[5]
        print u'ruchy do środka:\t', stats[6]

    try:
        options, args = getopt.getopt(argv[1:], 'hc:e:i:m:s:t:w:',
                ['help'])
        for option, argument in options:
            if option in ('-h', '--help'):
                print __doc__.split('\n')[0]
                print usage
                sys.exit()
            elif option == '-c':
                p_c = float(argument)
            elif option == '-e':
                encoding = argument
            elif option == '-i':
                iterations = int(argument)
            elif option == '-m':
                p_m = float(argument)
            elif option == '-s':
                population_size = int(argument)
            elif option == '-t':
                tournament_size = int(argument)
            elif option == '-w':
                words = int(argument)

        if args:
            print u'Analiza statystyczna tekstu...'
            corpus = Corpus(args, encoding, words)

            print u'Najlepsze przystosowanie w kolejnych populacjach:'
            results = []
            for i, best in enumerate(epoch(iterations, population_size,
                p_c, p_m, select_tournament, (tournament_size,), fitness,
                (corpus,))):
                results.append(best)
                print i+1, '\t', best.fitness

            print u'\nLOSOWY UKŁAD KLAWIATURY:'
            random_layout = Specimen(fitness, (corpus,))
            print_stats(random_layout, corpus)
            print u'\nQWERTY:'
            qwerty = Specimen(fitness, (corpus,), clone = \
                    u"q w e r t y u i o p".split() + \
                    u"a s d f g h j k l ;".split() + \
                    u"z x c v b n m , . ?".split())
            print_stats(qwerty, corpus)
            print u'\nDVORAK:'
            dvorak = Specimen(fitness, (corpus,), clone = \
                    u"' , . p y f g c r l".split() + \
                    u"a o e u i d h t n s".split() + \
                    u"; q j k x b m w v z".split())
            print_stats(dvorak, corpus)
            print u'\nWYNIK ALGORYTMU GENETYCZNEGO:'
            print_stats(min(results), corpus)

        else:
            print usage
    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)
    except IOError:
        print >> sys.stderr, u'błąd: nie można odnaleźć pliku'
        sys.exit(1)
    except UnicodeDecodeError:
        print >> sys.stderr, u'błąd: plik zawiera znaki spoza kodowania', \
                encoding
        sys.exit(1)


def fitness(specimen, corpus):
    u"""Funkcja przystosowania.

    :Parameters:
        - `specimen`: Układ klawiatury, którego przystosowanie obliczamy.
        - `corpus`: Częstotliwości występowania słów w tekście, na podstawie
          których obliczana jest wartość przystosowania układu klawiatury.

    :Return:
        - Ocena danego układu klawiatury. Im niższa, tym lepsza.

    """

    # tabela kar przyznawanych za klawisze na danych pozycjach
    costs = ( (5, 3, 3, 3, 4, 4, 3, 3, 3, 5),
              (1, 0, 0, 0, 2, 2, 0, 0, 0, 1),
              (6, 5, 5, 5, 7, 7, 5, 5, 5, 6) )
    punishment = 0 # punkty karne za nieoptymalne znaki
    same_hand_twice = False # poprzednie dwa znaki były napisane tą samą ręką
    prev_row, prev_col = None, None
    for word, freq in corpus.frequencies.iteritems():
        prev_row = None
        prev_col = None
        for c in word: # iterujemy kolejno po znakach
            if c not in specimen.genotype:
                prev_row = None
                prev_col = None
                continue
            # znajdujemy pozycję znaku na danym układzie klawiatury
            for i, r in enumerate(specimen.phenotype):
                if c in r:
                    row = i
                    col = r.index(c)
                    break
            # punkty karne zgodnie z tablicą kosztów
            punishment += costs[row][col] * freq
            if prev_row is not None and prev_col is not None:
                # punkty karne za pisanie tym samym palcem co poprzednio
                if (col == prev_col or (prev_col == 3 and col == 4) or \
                        (prev_col == 4 and col == 3) or \
                        (prev_col == 5 and col == 6) or \
                        (prev_col == 6 and col == 5)) and \
                        not (col == prev_col and row == prev_row):
                    punishment += 10 * freq
                # punkty karne za pisanie tą samą ręką co poprzednio
                if (prev_col < 5 and col < 5) or (prev_col >= 5 and col >= 5):
                    punishment += 1 * freq
                    if prev_row != row or (prev_row == 2 and row == 2):
                        punishment += 2 * freq
                    if row == prev_row and abs(col - prev_col) > 1:
                        punishment += 8 * freq
                    # punkty karne za ruchy od środka
                    if row == prev_row and abs(col - prev_col) == 1:
                        if (col < 5 and prev_col < 5) and (col < prev_col):
                            punishment += 8 * freq
                        elif (col >= 5 and prev_col >= 5) and (col > prev_col):
                            punishment += 8 * freq
                # punkty karne za trzy i więcej znaków napisanych tą samą ręką
                if (prev_col < 5 and col < 5) or (prev_col >= 5 and col >= 5):
                    if same_hand_twice:
                        punishment += 1 * freq
                    same_hand_twice = True
                else:
                    same_hand_twice = False
            else:
                same_hand_twice = False
            # punkty karne za pisanie lewą ręką
            if col < 5:
                punishment += 1 * freq
            # pamiętamy pozycję poprzedniego znaku w tekście
            prev_row = row
            prev_col = col
    return punishment


def statistics(specimen, corpus):
    u"""Zwróć statystyki dotyczące podanego układu klawiatury.

    Działa podobnie jak funkcja `fitness`, ale zwraca wyniki które są bardziej
    czytelne i zrozumiałe dla człowieka, podane procentowo.

    :Return:
        Napis zawierający:
            - Wykorzystanie rzędów klawiszy na klawiaturze.
            - Ilość znaków przepisana przez poszczególne palce.
            - Wykorzystanie lewej i prawej ręki.
            - Zmienianie rąk przy wpisywaniu kolejnych znaków.
            - Stosunek znaków napisanych innym palcem niż poprzednio do
              pozostałych.
            - Pisanie sąsiednich klawiszy w kierunku środka klawiatury.

    """
    all_chars = .0 # ilość znaków jakie uwzględniliśmy w analizie
    non_first_chars = .0 # ilość znaków, które nie wystąpują na początku słów
    stroke_flow = .0 # ilość znaków, które leżą obok poprzedniego znaku
    rows = [0, 0, 0] # rząd klawiszy
    fingers = 10*[0] # użycie palców
    hands = [0, 0] # użycie rąk
    alternation = 0 # zmiana rąk
    distance = 0 # odległość poprzedniego klawisza
    inboard_stroke_flow = 0 # ruchy do środka
    prev_row, prev_col = None, None
    for word, freq in corpus.frequencies.iteritems():
        prev_row = None
        prev_col = None
        for c in word: # iterujemy kolejno po znakach
            if c not in specimen.genotype:
                prev_row = None
                prev_col = None
                continue
            all_chars += 1 * freq
            if prev_col is not None:
                non_first_chars += 1 * freq
            # znajdujemy pozycję znaku na danym układzie klawiatury
            for i, r in enumerate(specimen.phenotype):
                if c in r:
                    row = i
                    col = r.index(c)
                    break
            # obliczamy udział rzędów klawiszy
            rows[row] += 1 * freq
            # obliczamy użycie palców
            fingers[col] += 1 * freq
            # obliczamy użycie rąk
            if col < 5:
                hands[0] += 1 * freq
            else:
                hands[1] += 1 * freq
            # obliczamy alternację rąk
            if prev_col is not None:
                if (prev_col < 5 and col >= 5) or (prev_col >= 5 and col < 5):
                    alternation += 1 * freq
            # obliczamy pisanie tym innym palcem niż poprzednio
            if prev_col is not None:
                if (col != prev_col and not (prev_col == 3 and col == 4) and \
                        not (prev_col == 4 and col == 3) and \
                        not (prev_col == 5 and col == 6) and \
                        not (prev_col == 6 and col == 5)) or \
                        col == prev_col and row == prev_row:
                    distance += 1 * freq
            # obliczamy ruchy do środka (inboard stroke flow)
            if prev_col is not None and prev_row is not None:
                if row == prev_row and abs(col - prev_col) == 1: # klawisz obok
                    if (col < 5 and prev_col < 5) or \
                            (col >= 5 and prev_col >= 5):
                        stroke_flow += 1
                    if (col < 5 and prev_col < 5) and (col > prev_col) or \
                            (col >= 5 and prev_col >= 5) and (col < prev_col):
                        inboard_stroke_flow += 1
            # pamiętamy pozycję poprzedniego znaku w tekście
            prev_row = row
            prev_col = col
    fingers[3] += fingers[4]
    fingers[6] += fingers[5]
    fingers = fingers[:4] + fingers[6:]
    string = ''
    for i in range(len(rows)):
        string += '%.1f' % (rows[i] / all_chars * 100) + '% '
    string = string[:-1] + '\nL: '
    for i in range(len(fingers) / 2):
        string += '%.f' % (fingers[i] / all_chars * 100) + '% '
    string += '\nR: '
    for i in range(len(fingers) / 2, len(fingers), 1):
        string += '%.f' % (fingers[i] / all_chars * 100) + '% '
    string = string[:-1] + '\n' + '%.1f' % (hands[0] / all_chars * 100) + \
            '% ' + '%.1f' % (hands[1] / all_chars * 100) + '%\n'
    string = string + '%.1f' % (alternation / non_first_chars * 100) + '%\n'
    string += '%.1f' % (distance / non_first_chars * 100) + '%\n'
    if stroke_flow > 0:
        string += '%.1f' % (inboard_stroke_flow / stroke_flow * 100) + '%\n'
    else:
        string += '?\n'
    return string


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
        - Pobieraj najlepiej przystosowane osobniki w kolejnych populacjach
          za pomocą generatora.

    """
    population = [] # lista osobników (instancji klasy Specimen)
    # tworzymy początkową populację złożoną z osobników o losowym genotypie
    for i in range(population_size):
        population.append(Specimen(fitness, fit_args))
    yield min(population)
    for i in range(iterations - 1):
        new_population = []
        for i in range(len(population)):
            parent1 = selection(population, *select_args)
            parent2 = selection(population, *select_args)
            parents = (parent1, parent2)
            offspring = Specimen(fitness, fit_args, parents, p_c, p_m)
            new_population.append(offspring)
        population = new_population
        yield min(population)


def select_tournament(population, k):
    u"""Selekcja turniejowa.

    Losuj bez powtórzeń `k` osobników z danej populacji i zwróć najlepiej
    przystosowanego. Uznajemy, że mniejsza wartość przystosowania jest lepsza.

    """
    return min(random.sample(population, k))


if __name__ == '__main__':
    main(sys.argv)

