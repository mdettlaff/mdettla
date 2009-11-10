#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

u"""Sprowadzanie formuł logicznych do koniunkcyjnej postaci normalnej (CNF).

Użycie: cnf.py < PLIK_WEJŚCIOWY > PLIK_WYJŚCIOWY

PLIK_WEJŚCIOWY zawiera formułę logiczną, a PLIK_WYJŚCIOWY równoważną formułę
w formacie DIMACS CNF.

"""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'
__date__ = '2009-11-10'


import sys


class Operator:
    def __init__(self, symbol, arity):
        self.symbol = symbol
        self.arity = arity

    def __eq__(self, other):
        return self.symbol == other.symbol and self.arity == other.arity

    def __str__(self):
        return self.symbol


# operatory logiczne
NOT = Operator('~', 1)
IMPL = Operator('=>', 2)
OR = Operator('V', 2)
AND = Operator('&', 2)


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


class FormulaCNF(Formula):

    def __init__(self, formula):
        self.isLiteral = formula.isLiteral
        if not formula.isLiteral:
            self.operator = formula.operator
            self.__args = ()
            for i in range(formula.operator.arity):
                self.__args += (FormulaCNF(formula[i]),)
        else:
            self.literal = formula.literal

    def __getitem__(self, index):
        return self.__args[index]

    def toDIMACS(self):
        dimacs, literals = self.__toDIMACSrecursive({})
        return 'p cnf ' + str(len(literals)) + ' ' + \
                str(dimacs.count('\n') + 1) + '\n' + dimacs

    def __toDIMACSrecursive(self, literals):
        s = ''
        if self.isLiteral:
            if not self.literal in literals:
                literals[self.literal] = len(literals) + 1
            s = str(literals[self.literal])
        elif self.operator == NOT:
            s = '-' + self[0].__toDIMACSrecursive(literals)[0]
        elif self.operator == OR:
            for i in range(self.operator.arity):
                s += self[i].__toDIMACSrecursive(literals)[0]
                if (i < len(self.__args) - 1):
                    s += ' '
        elif self.operator == AND:
            for i in range(self.operator.arity):
                s += self[i].__toDIMACSrecursive(literals)[0] + ' 0\n'
            s = s[:-1]
        return s, literals


class FormulaParser:

    OPERATORS = (NOT, IMPL, OR, AND)

    class Token:
        def __init__(self, token_string, is_operator):
            self.string = token_string
            self.isLiteral = not is_operator and token_string not in '()'

        def __str__(self):
            return self.string

    def __init__(self):
        self.token = None

    def parse(self, input):

        def tokenize(input):
            it = iter(input)
            for c in it:
                if not c.isspace():
                    is_operator_matched = False
                    for operator in FormulaParser.OPERATORS:
                        if operator.symbol.startswith(c):
                            is_operator_matched = True
                            for c_symbol in operator.symbol[1:]:
                                c = it.next()
                                if c_symbol != c:
                                    raise Exception(
                                            'Nie mogę sparsować operatora')
                            yield FormulaParser.Token(operator.symbol, True)
                    if not is_operator_matched:
                        yield FormulaParser.Token(c, False)

        # form0 ::= form1 | form1 '=>' form0
        def form0(tokens):
            form = form1(tokens)
            while not self.token.isLiteral and \
                    self.token.string == IMPL.symbol:
                self.token = tokens.next()
                form = Formula(IMPL, form, form0(tokens))
            return form

        # form1 ::= form2 | form1 'V' form2
        def form1(tokens):
            form = form2(tokens)
            while not self.token.isLiteral and self.token.string == OR.symbol:
                self.token = tokens.next()
                form = Formula(OR, form, form2(tokens))
            return form

        # form2 ::= form3 | form2 '&' form3
        def form2(tokens):
            form = form3(tokens)
            while not self.token.isLiteral and self.token.string == AND.symbol:
                self.token = tokens.next()
                form = Formula(AND, form, form3(tokens))
            return form

        # form3 ::= '~' form3 | form4
        def form3(tokens):
            if not self.token.isLiteral and self.token.string == NOT.symbol:
                self.token = tokens.next()
                form = form3(tokens)
                form = Formula(NOT, form)
            else:
                form = form4(tokens)
            return form

        # form4 ::= literal | '(' form0 ')'
        def form4(tokens):
            try:
                if self.token.isLiteral:
                    form = Formula.createLiteral(self.token.string)
                    self.token = tokens.next()
                elif self.token.string == '(':
                    self.token = tokens.next()
                    form = form0(tokens)
                    self.token = tokens.next()
            except StopIteration:
                pass
            return form

        tokens = tokenize(input)
        self.token = tokens.next()
        return form0(tokens)


def cnf(phi):

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

    def DISTR(eta0, eta1):
        # η₁ is η₁₁∧η₁₂: return DISTR(η₁₁, η₂)∧DISTR(η₁₂, η₂)
        if not eta0.isLiteral and eta0.operator == AND:
            return Formula(AND, DISTR(eta0[0], eta1), DISTR(eta0[1], eta1))
        # η₂ is η₂₁∧η₂₂: return DISTR(η₁, η₂₁)∧DISTR(η₁, η₂₂)
        elif not eta1.isLiteral and eta1.operator == AND:
            return Formula(AND, DISTR(eta0, eta1[0]), DISTR(eta0, eta1[1]))
        else:
            return Formula(OR, eta0, eta1)

    return CNF(NNF(IMPL_FREE(phi)))


def main():
    input = sys.stdin.readline()
    formula = FormulaParser().parse(input)
    print FormulaCNF(cnf(formula)).toDIMACS()


if __name__ == '__main__':
    main()

