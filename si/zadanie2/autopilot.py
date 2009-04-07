#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Automatyczny pilot - przykład sterownika rozmytego."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import sys


INFINITY = 255
h_integ = .1 # krok przy liczeniu całki (im mniejszy, tym lepsza dokładność)


class FuzzySet:
    u"""Zbiór rozmyty."""
    def degree(self, x):
        u"""Stopień zawierania się wartości `x` w tym zbiorze.
        
        :Return:
            Stopień zawierania się. Przyjmuje wartości między 0 i 1 włącznie.
            
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
        deg = 0
        if x <= self.a:
            deg = 0
        elif x <= self.b:
            deg = (x-self.a) / (self.b-self.a)
        elif x <= self.c:
            deg = (self.c-x) / (self.c-self.b)
        else:
            deg = 0
        if hasattr(self, 'multiplier') and self.multiplier is not None:
            return deg * self.multiplier(x)
        else:
            return deg


class LimitedSet(TriangleSet):
    def __init__(self, triangle_set, limit):
        self.a = triangle_set.a
        self.b = triangle_set.b
        self.c = triangle_set.c
        self.limit = limit

    def degree(self, x):
        return min(TriangleSet.degree(self, x), self.limit)


class MultiSet(FuzzySet):
    u"""Suma zbiorów rozmytych."""
    def __init__(self, sets):
        self.sets = sets

    def degree(self, x):
        return max([set.degree(x) for set in self.sets])


def integral(set, func, a, b, h):
    u"""Całkowanie numeryczne metodą trapezów."""
    integ = 0
    x = a
    while x < b:
        integ += h/2 * (func(set, x) + func(set, x + h))
        x += h
    return integ


def main(argv):
    print __doc__

    # funkcja przyjmująca wartość 0 dla x <= 0 i 1 gdy x > 0
    def positive(x):
        if x > 0:
            return 1
        else:
            return 0

    # h
    NZ = TriangleSet(0, 0, 500)
    S = TriangleSet(-200, 300, 800, positive)
    M = TriangleSet(300, 800, 1300, lambda x: 1 - positive(x - 1000))
    L = TriangleSet(500, 1000, 1000)

    # v, f
    DL = TriangleSet(-INFINITY, -20, -10)
    DS = TriangleSet(-20, -10, 0)
    Z = TriangleSet(-10, 0, 10)
    US = TriangleSet(0, 10, 20)
    UL = TriangleSet(10, 20, INFINITY)

    v_sets = [DL, DS, Z, US, UL]
    h_sets = [L, M, S, NZ]

    FAM = [ [ Z, DS, DL, DL, DL],
            [US,  Z, DS, DL, DL],
            [UL, US,  Z, DS, DL],
            [UL, UL,  Z, DS, DS] ]

    v_next = lambda t, f: v_next(t-1) + f
    h_next = lambda t, f: h_next(t-1) + v_next(t, f)

    t = 0
    h = 1000
    v = -20

    conclusions = {} # {zbiór: stopień przynależności}
    for j, h_set in enumerate(h_sets):
        for i, v_set in enumerate(v_sets):
            deg_v = v_set.degree(v) # stopień przynależności do zbioru
            deg_h = h_set.degree(h) # stopień przynależności do zbioru
            if deg_v and deg_h:
                if FAM[j][i] not in conclusions:
                    conclusions[FAM[j][i]] = min(deg_v, deg_h)
                else:
                    conclusions[FAM[j][i]] = \
                            max(conclusions[FAM[j][i]], min(deg_v, deg_h))
    for conclusion, deg in conclusions.iteritems():
        print conclusion, deg
    # wynikowy zbiór rozmyty
    sum_conclusion = MultiSet([LimitedSet(c, limit) for \
        c, limit in conclusions.iteritems()])
    print 'zbior_wynikowy(10) =', sum_conclusion.degree(10)
    # wyostrzanie
    i2 = integral(sum_conclusion, MultiSet.degree, \
            -INFINITY, INFINITY, h_integ)
    i1 = integral(sum_conclusion, \
            lambda self, x: MultiSet.degree(self, x) * x, \
            -INFINITY, INFINITY, h_integ)
    center_of_gravity = i1/i2
    print 'f = %.2f' % center_of_gravity


if __name__ == '__main__':
    main(sys.argv)

