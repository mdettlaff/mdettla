#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Automatyczny pilot - przykład sterownika rozmytego."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import getopt
import sys


usage = """\
Użycie: python autopilot.py [opcje]
Opcje:
    -d H  Krok H przy wyostrzaniu (mniejsza wartość daje lepszą dokładność).
    -i N  Wykonaj N iteracji algorytmu.
    -m    Wyostrzanie metodą średniego maksimum (zamiast środka ciężkości).
    -p    Problem odwróconego wahadła.\
"""

DEFAULT_ITERATIONS_AUTOPILOT = 200
DEFAULT_ITERATIONS_PENDULUM = 4
DEFAULT_DEFUZ_PRECISION = .1
u"""Dokładność przy wyostrzniu zbiorów (mniejsza wartość jest lepsza)."""
INFINITY = 65535


class FuzzySet:
    u"""Zbiór rozmyty."""
    def membership(self, x):
        u"""Zwróć stopień przynależności wartości `x` do tego zbioru.

        :Return:
            Stopień przynależności. Przyjmuje wartości od 0 do 1 włącznie.

        """

    def maximum(self):
        u"""Maksymalna wartość przynależności przyjmowana przez ten zbiór."""


class TriangleSet(FuzzySet):
    u"""Zbiór rozmyty zdefiniowany przez trójkątną funkcję przynależności."""
    def __init__(self, a, b, c, multiplier=None):
        self.a = a
        self.b = b
        self.c = c
        self.multiplier = multiplier

    def membership(self, x):
        u"""Trójkątna funkcja przynależności."""
        x = float(x)
        membership = 0
        if self.a <= x <= self.b:
            if self.a != -INFINITY:
                membership = (x - self.a) / (self.b - self.a)
            else:
                membership = 1
        elif self.b <= x <= self.c:
            if self.c != INFINITY:
                membership = (self.c - x) / (self.c - self.b)
            else:
                membership = 1
        if hasattr(self, 'multiplier') and self.multiplier is not None:
            return membership * self.multiplier(x)
        else:
            return membership

    @property
    def maximum(self):
        return 1


class LimitedSet(FuzzySet):
    u"""Zbiór rozmyty ograniczony z góry przez podaną prostą."""
    def __init__(self, set, limit):
        u"""Utwórz zbiór przez ograniczenie zbioru `set` prostą y = `limit`."""
        self.base_set = set
        self.limit = limit

    def membership(self, x):
        return min(self.base_set.membership(x), self.limit)

    @property
    def maximum(self):
        return self.limit


class MultiSet(FuzzySet):
    u"""Suma zbiorów rozmytych."""
    def __init__(self, sets):
        self.sets = sets

    def membership(self, x):
        return max([set.membership(x) for set in self.sets])

    @property
    def maximum(self):
        return max([set.maximum for set in self.sets])


def integral(f, a, b, h):
    u"""Całkowanie numeryczne metodą trapezów."""
    integral = 0
    x = a + h
    while x < b:
        integral += f(x)
        x += h
    return h * (integral + .5 * (f(a) + f(b)))


def defuzzify_center_of_gravity(fuzzy_set, u_min, u_max, h):
    u"""Wyostrzanie zbioru rozmytego metodą środka ciężkości."""
    i1 = integral(lambda x: fuzzy_set.membership(x) * x, u_min, u_max, h)
    i2 = integral(fuzzy_set.membership, u_min, u_max, h)
    return i1 / i2


def defuzzify_middle_of_maximum(fuzzy_set, u_min, u_max, h):
    u"""Wyostrzanie zbioru rozmytego metodą średniego maksimum."""
    left_max_found = False
    left_max = u_min
    right_max = u_min
    x = u_min
    while x < u_max:
        if fuzzy_set.membership(x) == fuzzy_set.maximum:
            if not left_max_found:
                left_max = x
            left_max_found = True
            right_max = x
        x += h
    return left_max + abs(left_max - right_max) / 2


def fuzzy_controller(x1_sets, x2_sets, FAM, x1_init, x2_init, x1_next, x2_next,
        defuzzify, h, iterations, u_min, u_max):
    u"""Sterownik rozmyty.

    :Parameters:
        - `FAM`: Rozmyta pamięć asocjacyjna.
        - `defuzzify`: Funkcja wyostrzająca wynikowy zbiór rozmyty.
        - `h`: Krok przy wyostrzaniu (im mniejszy, tym lepsza dokładność).
        - `iterations`: Liczba iteracji algorytmu.
        - `u_min`: Minimum dziedziny zbioru rozmytego.
        - `u_max`: Maksimum dziedziny zbioru rozmytego.

    :Return:
        Zwracaj za pomocą generatora:
            - Wartość x1 w danej iteracji.
            - Wartość x2 w danej iteracji.
            - Wartość sterowania u w danej iteracji.

    """
    x1 = x1_init
    x2 = x2_init
    for iter in range(iterations):
        conclusions = {} # klucz: zbiór, wartość: stopień przynależności
        for j, x1_set in enumerate(x1_sets):
            for i, x2_set in enumerate(x2_sets):
                deg_x1 = x1_set.membership(x1) # stopień przynależności
                deg_x2 = x2_set.membership(x2) # stopień przynależności
                if deg_x2 and deg_x1:
                    # agregacja
                    conclusions[FAM[j][i]] = min(deg_x2, deg_x1) if \
                            FAM[j][i] not in conclusions else \
                            max(conclusions[FAM[j][i]], min(deg_x2, deg_x1))
        # aktywacja i akumulacja
        # wynikowy zbiór rozmyty
        sum_conclusion = MultiSet([LimitedSet(c, limit) for \
                c, limit in conclusions.iteritems()])
        # wyostrzanie metodą środka ciężkości
        u = defuzzify(sum_conclusion, u_min, u_max, h)

        yield (x1, x2, u)
        x1_prev = x1
        x1 = x1_next(x1_prev, x2)
        x2 = x2_next(x1_prev, x2, u)


def autopilot(defuzzify, defuz_precision, iterations):
    u"""Symulacja lądowania samolotu przez automatycznego pilota."""

    positive = lambda x: 1 if x > 0 else 0

    NZ = TriangleSet(0, 0, 500)
    S = TriangleSet(-200, 300, 800, positive)
    M = TriangleSet(300, 800, 1300, lambda x: 1 - positive(x - 1000))
    L = TriangleSet(500, 1000, 1000)

    DL = TriangleSet(-INFINITY, -20, -10)
    DS = TriangleSet(-20, -10, 0)
    Z = TriangleSet(-10, 0, 10)
    US = TriangleSet(0, 10, 20)
    UL = TriangleSet(10, 20, INFINITY)

    h_sets = [L, M, S, NZ]
    v_sets = [DL, DS, Z, US, UL]

    FAM = [ [ Z, DS, DL, DL, DL],
            [US,  Z, DS, DL, DL],
            [UL, US,  Z, DS, DL],
            [UL, UL,  Z, DS, DS] ]

    h_next = lambda h, v: h + v
    v_next = lambda h, v, f: v + f

    h_init = 1000
    v_init = -20

    return fuzzy_controller(h_sets, v_sets, FAM, h_init, v_init,
            h_next, v_next, defuzzify, defuz_precision, iterations, -20, 20)


def pendulum(defuzzify, defuz_precision, iterations):
    u"""Problem odwróconego wahadła."""

    NEG1 = TriangleSet(-INFINITY, -2, 0)
    Z1 = TriangleSet(-2, 0, 2)
    POS1 = TriangleSet(0, 2, INFINITY)
    NEG2 = TriangleSet(-INFINITY, -5, 0)
    Z2 = TriangleSet(-5, 0, 5)
    POS2 = TriangleSet(0, 5, INFINITY)

    NB = TriangleSet(-INFINITY, -24, -16)
    N = TriangleSet(-16, -8, 0)
    Z = TriangleSet(-8, 0, 8)
    P = TriangleSet(0, 8, 16)
    PB = TriangleSet(16, 24, INFINITY)

    x1_sets = [NEG1, Z1, POS1]
    x2_sets = [NEG2, Z2, POS2]
    FAM = [ [NB, N,  Z],
            [ N, Z,  P],
            [ Z, P, PB] ]

    x1_next = lambda x1, x2: x1 + x2
    x2_next = lambda x1, x2, u: x1 + x2 - u

    x1_init = 1
    x2_init = -4

    return fuzzy_controller(x1_sets, x2_sets, FAM, x1_init, x2_init,
            x1_next, x2_next, defuzzify, defuz_precision, iterations, -24, 24)


def main(argv):
    defuzzification = defuzzify_center_of_gravity
    defuz_precision = DEFAULT_DEFUZ_PRECISION
    iterations = None
    try:
        options, args = getopt.getopt(argv[1:], 'hd:i:mp', ['help'])
        for option, argument in options:
            if option in ('-h', '--help'):
                print __doc__
                print usage
                sys.exit()
            elif option == '-d':
                defuz_precision = float(argument)
            elif option == '-i':
                iterations = int(argument)
            elif option == '-m':
                defuzzification = defuzzify_middle_of_maximum
            elif option == '-p':
                if not iterations:
                    iterations = DEFAULT_ITERATIONS_PENDULUM
                print 'x1\t\tx2\t\tu'
                for x1, x2, u in pendulum(defuzzification, defuz_precision,
                        iterations):
                    print '%8.3f\t%8.4f\t%8.5f' % (x1, x2, u)
                sys.exit()
        if not iterations:
            iterations = DEFAULT_ITERATIONS_AUTOPILOT

        print 'h\t\tv\t\tf'
        for h, v, f in autopilot(defuzzification, defuz_precision, iterations):
            print '%8.3f\t%8.4f\t%8.5f' % (h, v, f)

    except getopt.GetoptError, err:
        print str(err)
        print usage
        sys.exit(2)


if __name__ == '__main__':
    main(sys.argv)

