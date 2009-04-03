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

Funkcja oceny (przystosowania) obliczana jest za pomocą wzoru

    c1*w1 + c2*w2 + ... + cN*wN,

gdzie c1,...,cN oznaczają cechy układu klawiatury, a w1,...,wN to ich wagi,
decydujące o wpływie danej cechy na ogólną ocenę. Im większa wartość funkcji
oceny, tym lepsze przystosowanie osobnika. Wartości liczbowe są przypisywane
cechom na podstawie analizy reprezentatywnego zbioru tekstów.

Pod uwagę brane są następujące cechy:

1. Rząd klawiszy. Za literę w środkowym rzędzie wartość zwiększana jest o 1, \
w górnym o 0.5, a w dolnym 0. Ruch palca wskazującego do środka: +0.5.
2. Użycie palców. Za literę wpisaną małym palcem 0, serdecznym +0.5, reszta +1.
3. Zmiana rąk. Za literę wpisaną inną ręką niż poprzednio +1.
4. Odległość poprzedniego klawisza. Za literę wpisaną tym samym palcem co \
poprzednio przyznajemy 0, palcem obok +0.5. W pozostałych przypadkach +1.

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
    -e ENC  Kodowanie znaków w plikach wejściowych (domyślnie UTF-8).
    -c P    Prawdopodobieństwo krzyżowania (0 <= P <= 1).
    -m P    Prawdopodobieństwo mutacji (0 <= P <= 1).
    -t K    Rozmiar turnieju K w selekcji turniejowej.
    -s ROZ  ROZMIAR (ilość osobników) populacji.
    -w N    Analizuj tylko N najczęściej występujących wyrazów.
    -r W    Waga cechy rząd klawiszy.
    -u W    Waga cechy użycie palców.
    -a W    Waga cechy alternacja rąk.
    -d W    Waga cechy odległość poprzedniego klawisza.
    --help  Wyświetl treść pomocy i zakończ.\
"""

DEFAULT_ITERATIONS = 32
u"""Domyślna ilość iteracji (pokoleń) algorytmu genetycznego."""
DEFAULT_POPULATION_SIZE = 100
u"""Domyślny rozmiar populacji."""
DEFAULT_TOURNAMENT_SIZE = 4
u"""Domyślny rozmiar turnieju dla selekcji turniejowej."""
DEFAULT_P_C = .7
u"""Domyślne prawdopodobieństwo krzyżowania."""
DEFAULT_P_M = .7
u"""Domyślne prawdopodobieństwo mutacji."""
DEFAULT_WEIGHTS = [1, 1, 1, 1]
u"""Domyślne wartości wag dla cech układów klawiatury."""
DEFAULT_WORDS = 128
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
    weights = DEFAULT_WEIGHTS
    words = DEFAULT_WORDS
    encoding = DEFAULT_ENCODING

    def print_stats(specimen, corpus):
        print specimen
        print u'przystosowanie:\t\t', specimen.fitness
        stats = statistics(specimen, corpus).split('\n')
        print u'rzędy klawiszy:\t\t', stats[0]
        print u'palce lewej ręki:\t', stats[1][3:]
        print u'palce prawej ręki:\t', stats[2][3:]
        print u'alternacja rąk:\t\t', stats[3]
        print u'zmiana palca:\t\t', stats[4]

    try:
        options, args = getopt.getopt(argv[1:], 'he:s:i:c:m:w:r:u:a:d:',
                ['help'])
        for option, argument in options:
            if option in ('-h', '--help'):
                print __doc__.split('\n')[0]
                print usage
                sys.exit()
            elif option == '-e':
                encoding = argument
            elif option == '-i':
                iterations = int(argument)
            elif option == '-c':
                p_c = float(argument)
            elif option == '-m':
                p_m = float(argument)
            elif option == '-t':
                tournament_size = int(argument)
            elif option == '-s':
                population_size = int(argument)
            elif option == '-w':
                words = int(argument)
            elif option == '-r':
                weights[0] = float(argument)
            elif option == '-u':
                weights[1] = float(argument)
            elif option == '-a':
                weights[2] = float(argument)
            elif option == '-d':
                weights[3] = float(argument)

        if args:
            print u'Analiza statystyczna tekstu...'
            corpus = Corpus(args, encoding, words)

            print u'Najlepsze przystosowanie w kolejnych populacjach:'
            results = []
            for i, best in enumerate(epoch(iterations, population_size,
                p_c, p_m, select_tournament, [tournament_size], fitness,
                [corpus] + weights)):
                results.append(best)
                print '%d\t%.2f' % (i+1, best.fitness)

            print u'LOSOWY UKŁAD KLAWIATURY:'
            random_layout = Specimen(fitness, [corpus] + weights)
            print_stats(random_layout, corpus)
            print u'\nQWERTY:'
            qwerty = Specimen(fitness, [corpus] + [1, 1, 1 ,1], clone = \
                    u"q w e r t y u i o p".split() + \
                    u"a s d f g h j k l ;".split() + \
                    u"z x c v b n m , . ?".split())
            print_stats(qwerty, corpus)
            print u'\nDVORAK:'
            dvorak = Specimen(fitness, [corpus] + weights, clone = \
                    u"' , . p y f g c r l".split() + \
                    u"a o e u i d h t n s".split() + \
                    u"; q j k x b m w v z".split())
            print_stats(dvorak, corpus)
            print u'\nWYNIK ALGORYTMU GENETYCZNEGO:'
            print_stats(max(results), corpus)

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


def fitness(specimen, corpus, rows_weight=1, fingers_weight=1,
        alternation_weight=1, distance_weight=1):
    u"""Funkcja przystosowania.

    Przy obliczaniu funkcji przystosowania bierzemy pod uwagę następujące cechy
    układu klawiatury: rząd klawisza, użycie palców (dłuższe palce powinny
    wykonywać więcej pracy), alternacja rąk, odległość poprzedniego klawisza.
    O wpływie danej cechy na wynik decydują wagi (patrz parametry).

    :Parameters:
        - `specimen`: Układ klawiatury, którego przystosowanie obliczamy.
        - `corpus`: Częstotliwości występowania słów w tekście, na podstawie
          których obliczana jest wartość przystosowania układu klawiatury.
        - `rows_weight`: Waga cechy rząd klawisza.
        - `fingers_weight`: Waga cechy użycie palców.
        - `alternation_weight`: Waga cechy alternacja rąk.
        - `distance_weight`: Waga cechy odległość od poprzedniego klawisza.

    :Return:
        - Ocena danego układu klawiatury. Im wyższa, tym lepsza.

    """

    rows = 0 # cecha: rząd klawiszy
    fingers = 0 # cecha: użycie palców (dłuższy palec powinien więcej pisać)
    alternation = 0 # cecha: zmiana rąk
    distance = 0 # cecha: odległość poprzedniego klawisza
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
            # obliczamy cechę: rząd klawiszy
            reward = 0
            if row == 1 and (3 < col < 6): # 'gh' na QWERTY
                reward = .5
            elif row == 1: # środkowy (home row)
                reward = 1
            elif row == 0 and (3 < col < 6): # 'ty' na QWERTY
                reward = .2
            elif row == 0: # górny
                reward = .5
            elif row == 2 and col == 4: # 'b' na QWERTY
                reward = 0
            elif row == 2: # dolny
                reward = .1
            rows += reward * freq
            # obliczamy cechę: użycie palców
            reward = 0
            if (col == 0 or col == 9) and (row != 1):
                reward = 0 # mały palec
            if (col == 0 or col == 9) and (row == 1):
                reward = .5 # mały palec, środkowy rząd
            elif (col == 1 or col == 8) and (row != 1):
                reward = .5 # palec serdeczny
            else:
                reward = 1 # palec środkowy lub wskazujący
            fingers += reward * freq
            # obliczamy cechę: alternacja rąk
            if prev_col is not None:
                if (prev_col < 5 and col >= 5) or (prev_col >= 5 and col < 5):
                    alternation += 1 * freq
            # obliczamy cechę: odległość poprzedniego klawisza
            if prev_row is not None and prev_col is not None:
                reward = 1 # ile dodać do distance
                if col == prev_col:
                    reward -= 1
                elif abs(col - prev_col) == 1: # klawisz obok
                    reward -= .3
                # skok z góry na dół lub odwrotnie
                #if abs(row - prev_row) == 2:
                #    reward -= .5
                distance += reward * freq
            # pamiętamy pozycję poprzedniego znaku w tekście
            prev_row = row
            prev_col = col
    return (rows * rows_weight) + (fingers * fingers_weight) + \
            (alternation * alternation_weight) + (distance * distance_weight)


def statistics(specimen, corpus):
    u"""Zwróć statystyki dotyczące podanego układu klawiatury.

    Działa podobnie jak funkcja `fitness`, ale zwraca wyniki które są bardziej
    czytelne i zrozumiałe dla człowieka, podane procentowo (więcej = lepiej).

    :Return:
        Napis zawierający:
            - Wykorzystanie rzędów klawiszy na klawiaturze.
            - Ilość znaków przepisana przez poszczególne palce.
            - Zmienianie rąk przy wpisywaniu kolejnych znaków.
            - Stosunek znaków napisanych innym palcem niż poprzednio do
              pozostałych.

    """
    all_chars = .0 # ilość znaków jakie uwzględniliśmy w analizie
    non_first_chars = .0 # znaki, które nie wystąpują na początku słów
    rows = [0, 0, 0] # rząd klawiszy
    fingers = 10*[0] # użycie palców
    alternation = 0 # zmiana rąk
    distance = 0 # odległość poprzedniego klawisza
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
            # obliczamy alternację rąk
            if prev_col is not None:
                if (prev_col < 5 and col >= 5) or (prev_col >= 5 and col < 5):
                    alternation += 1 * freq
            # obliczamy pisanie tym innym palcem niż poprzednio
            if prev_col is not None:
                if col != prev_col and not (prev_col == 3 and col == 4) and \
                        not (prev_col == 4 and col == 3) and \
                        not (prev_col == 5 and col == 6) and \
                        not (prev_col == 6 and col == 5):
                    distance += 1 * freq
            # pamiętamy pozycję poprzedniego znaku w tekście
            prev_row = row
            prev_col = col
    fingers[3] += fingers[4]
    fingers[6] += fingers[5]
    fingers = fingers[:4] + fingers[6:]
    string = ''
    for i in range(len(rows)):
        string += str(rows[i] / all_chars * 100)[:4] + '% '
    string = string[:-1] + '\nL: '
    for i in range(len(fingers) / 2):
        string += str(int(fingers[i] / all_chars * 100)) + '% '
    string += '\nR: '
    for i in range(len(fingers) / 2, len(fingers), 1):
        string += str(int(fingers[i] / all_chars * 100)) + '% '
    string = string[:-1] + '\n' + str(alternation / non_first_chars * 100) \
            [:4] + '%\n'
    string += str(distance / non_first_chars * 100)[:4] + '%\n'
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
    yield max(population)
    for i in range(iterations - 1):
        new_population = []
        for i in range(len(population)):
            parent1 = selection(population, *select_args)
            parent2 = selection(population, *select_args)
            parents = (parent1, parent2)
            offspring = Specimen(fitness, fit_args, parents, p_c, p_m)
            new_population.append(offspring)
        population = new_population
        yield max(population)


def select_tournament(population, k):
    u"""Selekcja turniejowa.

    Losuj bez powtórzeń `k` osobników z danej populacji i zwróć najlepiej
    przystosowanego.

    """
    return max(random.sample(population, k))


if __name__ == '__main__':
    main(sys.argv)

