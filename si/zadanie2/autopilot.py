#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Automatyczny pilot - przykład sterownika rozmytego."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import sys


INFINITY = 65535


class FuzzySet:
    u"""Zbiór rozmyty."""
    def degree(self, x):
        u"""Zwróć stopień przynależności wartości `x` do tego zbioru.

        :Return:
            Stopień przynależności. Przyjmuje wartości od 0 do 1 włącznie.

        """
        pass


class TriangleSet(FuzzySet):
    u"""Zbiór rozmyty zdefiniowany przez trójkątną funkcję przynależności."""
    def __init__(self, a, b, c, multiplier=None):
        self.a = a
        self.b = b
        self.c = c
        self.multiplier = multiplier

    def __str__(self):
        return 'triangle(' + str(self.a) + ', ' + str(self.b) + ', ' + \
                str(self.c) + ')'

    def degree(self, x):
        u"""Trójkątna funkcja przynależności."""
        x = float(x)
        degree = 0
        if x <= self.a:
            degree = 0
        elif x <= self.b:
            if self.a != -INFINITY:
                degree = (x - self.a) / (self.b - self.a)
            else:
                degree = 1
        elif x <= self.c:
            if self.c != INFINITY:
                degree = (self.c - x) / (self.c - self.b)
            else:
                degree = 1
        else:
            degree = 0
        if hasattr(self, 'multiplier') and self.multiplier is not None:
            return degree * self.multiplier(x)
        else:
            return degree


class LimitedSet(FuzzySet):
    u"""Zbiór trójkątny ograniczony z góry przez prostą y=`limit`."""
    def __init__(self, set, limit):
        self.base_set = set
        self.limit = limit

    def degree(self, x):
        return min(self.base_set.degree(x), self.limit)


class MultiSet(FuzzySet):
    u"""Suma zbiorów rozmytych."""
    def __init__(self, sets):
        self.sets = sets

    def degree(self, x):
        return max([set.degree(x) for set in self.sets])


def integral(f, a, b, h):
    u"""Całkowanie numeryczne metodą trapezów."""
    integral = 0
    x = a
    while x < b:
        integral += h/2 * (f(x) + f(x + h))
        x += h
    return integral


def fuzzy_controller(x1_sets, x2_sets, FAM, x1_init, x2_init, x1_next, x2_next,
        iterations, u_min, u_max, h):
    u"""Sterownik rozmyty.

    :Parameters:
        - `FAM`: Rozmyta pamięć asocjacyjna.
        - `iterations`: Liczba iteracji algorytmu.
        - `u_min`: Minimum dziedziny zbioru rozmytego.
        - `u_max`: Maksimum dziedziny zbioru rozmytego.
        - `h`: Krok przy liczeniu całki (im mniejszy, tym lepsza dokładność).

    :Return:
        Zwracaj za pomocą generatora:
            - Wartość x1 w danej iteracji.
            - Wartość x2 w danej iteracji.
            - Wartość sterowania u w danej iteracji.

    """
    x1 = x1_init
    x2 = x2_init
    t = 0
    for iter in range(iterations):
        conclusions = {} # klucz: zbiór, wartość: stopień przynależności
        for j, x1_set in enumerate(x1_sets):
            for i, x2_set in enumerate(x2_sets):
                deg_x2 = x2_set.degree(x2) # stopień przynależności do zbioru
                deg_x1 = x1_set.degree(x1) # stopień przynależności do zbioru
                if deg_x2 and deg_x1:
                    if FAM[j][i] not in conclusions:
                        # agregacja
                        conclusions[FAM[j][i]] = min(deg_x2, deg_x1)
                    else:
                        conclusions[FAM[j][i]] = \
                            max(conclusions[FAM[j][i]], min(deg_x2, deg_x1))
        # aktywacja i akumulacja
        # wynikowy zbiór rozmyty
        sum_conclusion = MultiSet([LimitedSet(c, limit) for \
                c, limit in conclusions.iteritems()])
        # wyostrzanie metodą środka ciężkości
        i1 = integral(lambda x: sum_conclusion.degree(x) * x, \
                u_min, u_max, h)
        i2 = integral(sum_conclusion.degree, u_min, u_max, h)
        u = i1/i2

        yield (x1, x2, u)
        x1_prev = x1
        x1 = x1_next(x1, x2)
        x2 = x2_next(x1_prev, x2, u)
        t += 1


def main(argv):

    # funkcja przyjmująca wartość 0 dla x <= 0 i 1 gdy x > 0
    def positive(x):
        if x > 0:
            return 1
        else:
            return 0

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

    h0 = 1000
    v0 = -20

    for h, v, f in fuzzy_controller(h_sets, v_sets, FAM, h0, v0,
            h_next, v_next, 200, -20, 20, .1):
        print '%8.3f\t%8.4f\t%8.5f' % (h, v, f)


    """
    # odwrócone wahadło

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

    for x1, x2, u in fuzzy_controller(x1_sets, x2_sets, FAM, x1_init, x2_init,
            x1_next, x2_next, 4, -24, 24, .1):
        print '%8.3f\t%8.4f\t%8.5f' % (x1, x2, u)
    """


if __name__ == '__main__':
    main(sys.argv)

