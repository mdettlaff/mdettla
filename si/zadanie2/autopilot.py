#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Automatyczny pilot - przykład sterownika rozmytego."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'

import sys


INFINITY = 65535
h_integ = .1
u"""Krok przy liczeniu całki (im mniejszy, tym lepsza dokładność)."""


class FuzzySet:
    u"""Zbiór rozmyty."""
    def degree(self, x):
        u"""Stopień przynależności wartości `x` do tego zbioru.

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
        deg = 0
        if x <= self.a:
            deg = 0
        elif x <= self.b:
            if self.a != -INFINITY:
                deg = (x - self.a) / (self.b - self.a)
            else:
                deg = 1
        elif x <= self.c:
            if self.c != INFINITY:
                deg = (self.c - x) / (self.c - self.b)
            else:
                deg = 1
        else:
            deg = 0
        if hasattr(self, 'multiplier') and self.multiplier is not None:
            return deg * self.multiplier(x)
        else:
            return deg


class LimitedSet(TriangleSet):
    def __init__(self, triangle_set, limit):
        u"""Zbiór trójkątny ograniczony z góry przez prostą y=`limit`."""
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

    # funkcja przyjmująca wartość 0 dla x <= 0 i 1 gdy x > 0
    def positive(x):
        if x > 0:
            return 1
        else:
            return 0

    U_MIN = -20
    u'Minimum dziedziny zbioru rozmytego.'
    U_MAX = 20
    u'Maksimum dziedziny zbioru rozmytego.'

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

    t = 0
    h = 1000
    v = -20

    for wibble in range(200):
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
        # wynikowy zbiór rozmyty
        sum_conclusion = MultiSet([LimitedSet(c, limit) for \
            c, limit in conclusions.iteritems()])
        # wyostrzanie
        i1 = integral(sum_conclusion, \
                lambda self, x: MultiSet.degree(self, x) * x, \
                U_MIN, U_MAX, h_integ)
        i2 = integral(sum_conclusion, MultiSet.degree, \
                U_MIN, U_MAX, h_integ)
        center_of_gravity = i1/i2

        f = center_of_gravity
        print '%.5f\t%.5f\t%.5f' % (h, v, f)
        h = h + v
        v = v + f
        t += 1

    # debug
    """
    for conclusion, deg in conclusions.iteritems():
        print conclusion, deg
    for i in range(-15, 25):
        print 'w(' + str(i) + ')=' + str(sum_conclusion.degree(i)) + ','
    print
    print 'f = %.2f/%.2f = %.2f' % (i1, i2, center_of_gravity)
    print integral(Z, lambda self, x: TriangleSet.degree(self, x) * x, \
            0, INFINITY-1, h_integ)
    """


    """
    # wahadło

    U_MIN = -24
    u'Minimum dziedziny zbioru rozmytego.'
    U_MAX = 24
    u'Maksimum dziedziny zbioru rozmytego.'

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

    t = 0
    x1 = 1
    x2 = -4

    for wibble in range(4):
        conclusions = {} # {zbiór: stopień przynależności}
        for j, x1_set in enumerate(x1_sets):
            for i, x2_set in enumerate(x2_sets):
                deg_x2 = x2_set.degree(x2) # stopień przynależności do zbioru
                deg_x1 = x1_set.degree(x1) # stopień przynależności do zbioru
                if deg_x2 and deg_x1:
                    if FAM[j][i] not in conclusions:
                        conclusions[FAM[j][i]] = min(deg_x2, deg_x1)
                    else:
                        conclusions[FAM[j][i]] = \
                           max(conclusions[FAM[j][i]], min(deg_x2, deg_x1))
        # wynikowy zbiór rozmyty
        sum_conclusion = MultiSet([LimitedSet(c, limit) for \
            c, limit in conclusions.iteritems()])
        # wyostrzanie
        i1 = integral(sum_conclusion, \
                lambda self, x: MultiSet.degree(self, x) * x, \
                U_MIN, U_MAX, h_integ)
        i2 = integral(sum_conclusion, MultiSet.degree, \
                U_MIN, U_MAX, h_integ)
        center_of_gravity = i1/i2

        u = center_of_gravity
        print '%8.5f\t%8.5f\t%8.5f' % (x1, x2, u)
        x1_prev = float(x1)
        x1 = x1 + x2
        x2 = x1_prev + x2 - u
        t += 1
    """

    # debug
    """
    for conclusion, deg in conclusions.iteritems():
        print conclusion, deg
    x = -20
    while x < 20:
        print 'w(' + str(x) + ')=' + str(sum_conclusion.degree(x))
        x += .5
    print
    print 'u = %.2f/%.2f = %.2f' % (i1, i2, center_of_gravity)
    #print integral(Z, lambda self, x: TriangleSet.degree(self, x) * x, \
    #        0, INFINITY-1, h_integ)
    """

if __name__ == '__main__':
    main(sys.argv)

