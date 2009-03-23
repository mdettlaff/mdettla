#!/usr/bin/env python
# -*- coding: UTF-8 -*-

"""Algorytm GSAT, sprawdzający spełnialność formuły.

Podana formuła musi być w koniunkcyjnej postaci normalnej (CNF).
Przykład: (p V q) & (~q V r V ~s) & (s V ~t)

"""

import sys
import random
import copy


MAX_ITER = 255 # maksymalna liczba iteracji algorytmu


class Literal:
    """Atom - pojedynczy symbol lub jego zaprzeczenie."""
    def __init__(self, symbol, not_negated):
        self.symbol = symbol
        self.not_negated = not_negated

    def __str__(self):
        if self.not_negated:
            return self.symbol
        else:
            return '~' + self.symbol

    def eval(self, value):
        """Zwróć wartość atomu dla podanego wartościowania."""
        return (self.not_negated and value) or \
                (not self.not_negated and not value)


class Formula:
    """Formuła w postaci CNF."""
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
                    literal = Literal(literal_str[-1], False)
                else:
                    literal = Literal(literal_str[-1], True)
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

    def str_eval(self, eval):
        """Zwróć formułę dla danego wartościowania jako napis."""
        s = ''
        for disjunction in self.CNF:
            s += '('
            for literal in disjunction:
                if literal.not_negated:
                    s += str(int(eval.values[literal.symbol])) + ' V '
                else:
                    s += '~' + str(int(eval.values[literal.symbol])) + ' V '
            s = s[:-3]
            s += ') & '
        return s[:-3]

    def satisfied_clauses_count(self, eval):
        """Zwróć liczbę dysjunkcji spełnionych dla podanego wartościowania."""
        satisfied_clauses = 0
        for disjunction in self.CNF:
            for literal in disjunction:
                if literal.eval(eval.values[literal.symbol]):
                    satisfied_clauses += 1
                    break
        return satisfied_clauses

    def get_clauses_count(self):
        return len(self.CNF)

    clauses_count = property(fget=get_clauses_count)


class Evaluation:
    """Wartościowanie (prawda/fałsz) dla danego zestawu symboli."""
    def __init__(self, symbols):
        """Utwórz losowe wartościowanie dla podanego zestawu symboli."""
        self.values = {}
        for symbol in symbols:
            self.values[symbol] = random.choice([True, False])

    def __cmp__(self, other):
        return cmp(self.rank, other.rank)

    def __str__(self):
        s = ''
        for key, value in self.values.iteritems():
            s += str(key) + '=' + str(value) + ', '
        return s[:-2]


def gsat(formula):
    """Algorytm GSAT.
    
    Sprawdza czy istnieje wartościowanie spełniające podaną formułę.
    Nie gwarantuje znalezienia rozwiązania.
    Zwraca tuplę: (wartościowanie, liczba iteracji) lub None, jeśli nie
    znaleziono wartościowania spełniającego formułę.
    
    """
    eval = Evaluation(formula.symbols) # losowe wartościowanie
    for iter in range(MAX_ITER):
        evals = []
        for symbol in formula.symbols:
            new_eval = copy.deepcopy(eval)
            new_eval.values[symbol] = not new_eval.values[symbol]
            new_eval.rank = \
                    formula.satisfied_clauses_count(new_eval)
            evals.append(new_eval)
        eval = max(evals)
        if eval.rank == formula.clauses_count:
            return (eval, iter+1)
    return None


if __name__ == '__main__':
    print u'Algorytm GSAT, sprawdzający spełnialność formuły.'
    print u'Podaj formułę w koniunkcyjnej postaci normalnej (CNF).'
    print u'Przykład: (p V q) & (~q V r V ~s) & (s V ~t)'

    formula = Formula(raw_input())
    print formula

    results = gsat(formula)

    if results:
        eval = results[0]
        print u'Podana formuła jest spełniona dla wartościowania:'
        print formula.str_eval(eval)
        print eval
        print u'Liczba iteracji algorytmu:', results[1]
    else:
        print u'Nie znaleziono wartościowana spełniającego formułę.'

