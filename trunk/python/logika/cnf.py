#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Sprowadzanie formuł logicznych do koniunkcyjnej postaci normalnej (CNF)."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'
__date__ = '2009-11-09'


import sys


# symbole operatorów logicznych
NOT = '~'
IMPL = '=>'
OR = 'V'
AND = '&'


class Formula:
    def __init__(self, operator, *form):
        self.isLiteral = operator is None
        if self.isLiteral:
            self.literal = form[0]
        else:
            if operator.arity != len(form):
                raise Exception('Operator \"' + str(operator) + '\" wymaga ' +
                        operator.arity + ' argumentów, podano ' + len(form))
            self.operator = operator
            self.form = form

    def __str__(self):
        s = ''
        if self.isLiteral:
            s = self.literal
        elif self.operator.arity == 1:
            s = str(self.operator) + str(self.form[0])
        else:
            for i in range(self.operator.arity):
                s += str(self.form[i])
                if (i < len(self.form) - 1):
                    s += ' ' + str(self.operator) + ' '
        return s


class Operator:
    def __init__(self, symbol):
        self.symbol = symbol
        if symbol == NOT:
            self.arity = 1
        elif symbol == OR or symbol == AND or symbol == IMPL:
            self.arity = 2
        else:
            raise Exception('Nieznany operator: ' + symbol)

    def __str__(self):
        return self.symbol


def NNF(form):
    if form.isLiteral:
        return form
    # if φ is ¬¬φ₁: return NNF(φ₁)
    elif form.operator.symbol == NOT and not form.form[0].isLiteral \
            and form.form[0].operator.symbol == NOT:
        return NNF(form.form[0].form[0])
    # if φ is φ₁∧φ₂: return NNF(φ₁)∧NNF(φ₂)
    elif form.operator.symbol == AND:
        return Formula(Operator(AND), NNF(form.form[0]), NNF(form.form[1]))
    # if φ is φ₁∨φ₂: return NNF(φ₁)∨NNF(φ₂)
    elif form.operator.symbol == OR:
        return Formula(Operator(OR), NNF(form.form[0]), NNF(form.form[1]))
    # if φ is ¬(φ₁∧φ₂): return NNF(φ₁)∨NNF(φ₂)
    elif form.operator.symbol == NOT and not form.form[0].isLiteral \
            and form.form[0].operator.symbol == AND:
        return Formula(Operator(OR),
                NNF(Formula(Operator(NOT), form.form[0].form[0])),
                NNF(Formula(Operator(NOT), form.form[0].form[1])))
    # if φ is ¬(φ₁∨φ₂): return NNF(φ₁)∧NNF(φ₂)
    elif form.operator.symbol == NOT and not form.form[0].isLiteral \
            and form.form[0].operator.symbol == OR:
        return Formula(Operator(AND),
                NNF(Formula(Operator(NOT), form.form[0].form[0])),
                NNF(Formula(Operator(NOT), form.form[0].form[1])))
    else:
        return form


def main(argv):
    p = Formula(None, 'p')
    q = Formula(None, 'q')
    print 'p lub q =', Formula(Operator(OR), p, q)

    not_not_p = Formula(Operator(NOT), Formula(Operator(NOT), p))
    print 'NNF(~~p & q) =', NNF(Formula(Operator(AND), not_not_p, q))

    print 'NNF(~(p & q)) =', \
            NNF(Formula(Operator(NOT), Formula(Operator(AND), p, q)))


if __name__ == '__main__':
    main(sys.argv)

