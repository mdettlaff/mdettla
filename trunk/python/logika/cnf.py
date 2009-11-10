#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

u"""Sprowadzanie formuł logicznych do koniunkcyjnej postaci normalnej (CNF)."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'
__date__ = '2009-11-09'


import sys


class Operator:
    def __init__(self, symbol, arity):
        self.symbol = symbol
        self.arity = arity

    def __eq__(self, other):
        return self.symbol == other.symbol and self.arity == other.arity

    def __str__(self):
        return self.symbol


class Formula:
    def __init__(self, operator, *args):
        self.isLiteral = operator is None and len(args) == 1
        if not self.isLiteral:
            if operator.arity != len(args):
                raise Exception('Operator \"' + str(operator) + '\" wymaga ' +
                        str(operator.arity) + ' argumentów, podano ' +
                        str(len(args)))
            self.operator = operator
            self.__args = args
        else:
            self.literal = args[0]

    @staticmethod
    def createLiteral(symbol):
        return Formula(None, symbol)

    def __getitem__(self, index):
        return self.__args[index]

    def __str__(self):
        s = ''
        if self.isLiteral:
            s = self.literal
        elif self.operator.arity == 1:
            s = str(self.operator) + str(self[0])
        else:
            s += '('
            for i in range(self.operator.arity):
                s += str(self[i])
                if (i < len(self.__args) - 1):
                    s += ' ' + str(self.operator) + ' '
            s += ')'
        return s


# operatory logiczne
NOT = Operator('~', 1)
IMPL = Operator('=>', 2)
OR = Operator('V', 2)
AND = Operator('&', 2)


def IMPL_FREE(phi):
    if phi.isLiteral:
        return phi
    elif phi.operator == NOT:
        return Formula(NOT, IMPL_FREE(phi[0]))
    elif phi.operator == AND:
        return Formula(AND, IMPL_FREE(phi[0]), IMPL_FREE(phi[1]))
    elif phi.operator == OR:
        return Formula(OR, IMPL_FREE(phi[0]), IMPL_FREE(phi[1]))
    elif phi.operator == IMPL:
        return IMPL_FREE(Formula(OR, Formula(NOT, phi[0]), phi[1]))


def NNF(phi):
    if phi.isLiteral:
        return phi
    # if φ is ¬¬φ₁: return NNF(φ₁)
    elif phi.operator == NOT and not phi[0].isLiteral \
            and phi[0].operator == NOT:
        return NNF(phi[0][0])
    # if φ is φ₁∧φ₂: return NNF(φ₁)∧NNF(φ₂)
    elif phi.operator == AND:
        return Formula(AND, NNF(phi[0]), NNF(phi[1]))
    # if φ is φ₁∨φ₂: return NNF(φ₁)∨NNF(φ₂)
    elif phi.operator == OR:
        return Formula(OR, NNF(phi[0]), NNF(phi[1]))
    # if φ is ¬(φ₁∧φ₂): return NNF(¬φ₁)∨NNF(¬φ₂)
    elif phi.operator == NOT and not phi[0].isLiteral \
            and phi[0].operator == AND:
        return Formula(OR,
                NNF(Formula(NOT, phi[0][0])),
                NNF(Formula(NOT, phi[0][1])))
    # if φ is ¬(φ₁∨φ₂): return NNF(¬φ₁)∧NNF(¬φ₂)
    elif phi.operator == NOT and not phi[0].isLiteral \
            and phi[0].operator == OR:
        return Formula(AND,
                NNF(Formula(NOT, phi[0][0])),
                NNF(Formula(NOT, phi[0][1])))
    else:
        return phi


def CNF(phi):
    if phi.isLiteral:
        return phi
    # φ is φ₁∧φ₂ : return CNF(φ₁)∧CNF(φ₂)
    elif phi.operator == AND:
        return Formula(AND, CNF(phi[0]), CNF(phi[1]))
    # φ is φ₁∨φ₂ : return DISTR(CNF(φ₁), CNF(φ₂))
    elif phi.operator == OR:
        return DISTR(CNF(phi[0]), CNF(phi[1]))
    else:
        return phi


def DISTR(phi0, phi1):
    # η₁ is η₁₁∧η₁₂: return DISTR(η₁₁, η₂)∧DISTR(η₁₂, η₂)
    if not phi0.isLiteral and phi0.operator == AND:
        return Formula(AND, DISTR(phi0[0], phi1), DISTR(phi0[1], phi1))
    # η₂ is η₂₁∧η₂₂: return DISTR(η₁, η₂₁)∧DISTR(η₁, η₂₂)
    elif not phi1.isLiteral and phi1.operator == AND:
        return Formula(AND, DISTR(phi0, phi1[0]), DISTR(phi0, phi1[1]))
    else:
        return Formula(OR, phi0, phi1)


def cnf(phi):
    return CNF(NNF(IMPL_FREE(phi)))


def main(argv):
    p = Formula.createLiteral('p')
    q = Formula.createLiteral('q')
    # φ = (~(p => ~q) V (q V ~p))
    phi = Formula(OR,
            Formula(NOT, Formula(IMPL, p, Formula(NOT, q))),
            Formula(OR, q, Formula(NOT, p)))
    print 'phi\t\t\t=', phi
    print 'IMPL_FREE(phi)\t\t=', IMPL_FREE(phi)
    print 'NNF(IMPL_FREE(phi))\t=', NNF(IMPL_FREE(phi))
    print 'cnf(phi)\t\t=', cnf(phi)


if __name__ == '__main__':
    main(sys.argv)

