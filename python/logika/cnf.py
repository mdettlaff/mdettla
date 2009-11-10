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


# operatory logiczne
NOT = Operator('~', 1)
IMPL = Operator('=>', 2)
OR = Operator('V', 2)
AND = Operator('&', 2)

token = None


def parse_formula(input):
    global token

    OPERATORS = (NOT, IMPL, OR, AND)

    class Token:
        def __init__(self, token_string, is_operator):
            self.string = token_string
            self.isLiteral = not is_operator and token_string not in '()'

        def __str__(self):
            return self.string

    def tokenize(input):
        it = iter(input)
        for c in it:
            if not c.isspace():
                is_operator_matched = False
                for operator in OPERATORS:
                    if operator.symbol.startswith(c):
                        is_operator_matched = True
                        for c_symbol in operator.symbol[1:]:
                            c = it.next()
                            if c_symbol != c:
                                raise Exception('Nie mogę sparsować operatora')
                        yield Token(operator.symbol, True)
                if not is_operator_matched:
                    yield Token(c, False)

    tokens = tokenize(input)

    # debug
    #s = ''
    #for token in tokenize(input):
    #    s += str(token) + '_'
    #print s[:-1]

    token = tokens.next()

    # form1 ::= form2 | form2 '=>' form1
    def form1(tokens):
        global token
        form_1 = form2(tokens)
        while not token.isLiteral and token.string == IMPL.symbol:
            token = tokens.next()
            form_0 = form1(tokens)
            form_1 = Formula(IMPL, form_1, form_0)
        return form_1

    # form2 ::= form3 | form2 'V' form3
    def form2(tokens):
        global token
        form = form3(tokens)
        while not token.isLiteral and token.string == OR.symbol:
            token = tokens.next()
            form_1 = form3(tokens)
            form = Formula(OR, form, form_1)
        return form

    # form3 ::= form4 | form3 '&' form4
    def form3(tokens):
        global token
        form = form4(tokens)
        while not token.isLiteral and token.string == AND.symbol:
            token = tokens.next()
            form_1 = form4(tokens)
            form = Formula(AND, form, form_1)
        return form

    # form4 ::= '~' form4 | form5
    def form4(tokens):
        global token
        if not token.isLiteral and token.string == NOT.symbol:
            token = tokens.next()
            form = form4(tokens)
            form = Formula(NOT, form)
        else:
            form = form5(tokens)
        return form

    # form5 ::= literal | '(' form0 ')'
    def form5(tokens):
        global token
        try:
            if token.isLiteral:
                form = Formula.createLiteral(token.string)
                token = tokens.next()
            elif token.string == '(':
                token = tokens.next()
                form = form1(tokens)
                token = tokens.next()
        except StopIteration:
            pass
        return form

    return form1(tokens)


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


def main(argv):
    p = Formula.createLiteral('p')
    q = Formula.createLiteral('q')
    # φ = (~(p => ~q) V (q V ~p))
    #phi = Formula(OR,
    #        Formula(NOT, Formula(IMPL, p, Formula(NOT, q))),
    #        Formula(OR, q, Formula(NOT, p)))
    phi = parse_formula('(~(p => ~q) V (q V ~p))')
    print 'phi\t\t=', phi
    phi2cnf = cnf(phi)
    print 'cnf(phi)\t=', phi2cnf
    print phi2cnf.toDIMACS()


if __name__ == '__main__':
    main(sys.argv)

