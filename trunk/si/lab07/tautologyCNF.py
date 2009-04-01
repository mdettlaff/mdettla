#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Sprawdzanie czy podana formuła jest tautologią.

Formuła musi być w koniunkcyjnej postaci normalnej (CNF).
Przykład: (p V q) & (~q V r V ~s) & (s V ~t)

"""


class Literal:
    u"""Pojedynczy symbol lub jego zaprzeczenie."""
    def __init__(self, symbol, negated):
        self.symbol = symbol
        self.negated = negated

    def __str__(self):
        if self.negated:
            return '~' + self.symbol
        else:
            return self.symbol


class Formula:
    u"""Formuła w postaci CNF."""
    def __init__(self, input):
        self.CNF = []
        self.symbols = set()
        input = input.strip().replace(' ', '').\
                replace('(', '').replace(')', '')
        conjunctions = input.split('&')
        for conjunction in conjunctions:
            self.CNF.append([])
            disjunction = conjunction.split('V')
            for literal_str in disjunction:
                if literal_str.startswith('~'):
                    literal = Literal(literal_str[-1], True)
                else:
                    literal = Literal(literal_str[-1], False)
                self.CNF[-1].append(literal)
                self.symbols.add(literal_str[-1])

    def __str__(self):
        s = ''
        for disjunction in self.CNF:
            s += '('
            for literal in disjunction:
                s += str(literal) + ' V '
            s = s[:-3]
            s += ') & '
        return s[:-3]


def is_tautology(formula):
    u"""Sprawdza czy podana w CNF formuła jest tautologią.

    Korzystamy z twierdzenia:
    Formuła w Koniunkcyjnej Postaci Normalnej jest tautologią <=>
    każda alternatywa zawiera kontrarną parę literałów (np. p & ~p).

    """
    for disjunction in formula.CNF:
        contrary_found = False
        for i, literal in enumerate(disjunction):
            for j in range(i, len(disjunction)):
                if literal.symbol == disjunction[j].symbol:
                    if literal.negated == (not disjunction[j].negated):
                        contrary_found = True
        if not contrary_found:
            return False
    return True


if __name__ == '__main__':
    print u'Podaj formułę w KPN:'

    formula = Formula(raw_input())
    print formula

    print u'Podana formuła',
    print u'jest' if is_tautology(formula) else u'nie jest', u'tautologią.'

