#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Sprowadzanie formuł logicznych do koniunkcyjnej postaci normalnej."""

__docformat__ = 'restructuredtext pl'
__author__ = u'Michał Dettlaff'


# symbole operatorów logicznych
NOT = '~'
IMPL = '=>'
OR = 'V'
AND = '&'


class Expression:
    def __init__(self, literal=None, op=None, args=None):
        # Expression(literal=lit)
        if literal is not None and op is None and args is None:
            self.isLiteral = True
            self.literal = literal
        # Expression(op=oper, args=a)
        elif literal is None and op is not None and args is not None:
            self.isLiteral = False
            if op.argCount != len(args):
                raise Exception('Operator \"' + str(op) + '\" wymaga ' +
                        op.argCount + ' argumentów (podano ' + len(args) + ')')
            self.operator = op
            self.args = args
        else:
            raise Exception('Nieprawidłowa próba utworzenia wyrażenia.')

    def __str__(self):
        s = ''
        if self.isLiteral:
            s = self.literal
        elif len(self.args) == 1:
            s = str(self.operator) + str(self.args[0])
        else:
            for i in range(len(self.args)):
                s += str(self.args[i])
                if (i < len(self.args) - 1):
                    s += ' ' + str(self.operator) + ' '
        return s


class Operator:
    def __init__(self, symbol):
        self.symbol = symbol
        if symbol == NOT:
            self.argCount = 1
        elif symbol == OR or symbol == AND or symbol == IMPL:
            self.argCount = 2
        else:
            raise Exception('Nieznany operator: ' + symbol)

    def __str__(self):
        return self.symbol


def NNF(expr):
    if expr.isLiteral:
        return expr
    # if φ is ¬¬φ₁: return NNF(φ₁)
    elif expr.operator.symbol == NOT and not expr.args[0].isLiteral \
            and expr.args[0].operator.symbol == NOT:
        return NNF(expr.args[0].args[0])
    # if φ is φ₁∧φ₂: return NNF(φ₁)∧NNF(φ₂)
    elif expr.operator.symbol == AND:
        return Expression(op=Operator(AND),
                args=(NNF(expr.args[0]), NNF(expr.args[1])))


def main():
    p = Expression(literal='p')
    q = Expression(literal='q')
    print 'p lub q =', Expression(op=Operator(OR), args=(p, q))

    neg_p = Expression(op=Operator(NOT), args=(p,))
    double_neg_p = Expression(op=Operator(NOT), args=(neg_p,))
    print 'NNF(~~p & q) =', \
            NNF(Expression(op=Operator(AND), args=(double_neg_p, q)))


if __name__ == '__main__':
    main()

