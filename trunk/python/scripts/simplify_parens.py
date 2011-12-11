#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

import doctest
import re


class Parser:

    LEXEMES = ['AND', 'OR', '(', ')']
    OTHER = None

    def __init__(self, expr):
        self.expr = expr

    def tokenize(self):
        """
        >>> Parser('a AND b').tokenize()
        ['a ', 'AND', ' b']
        >>> Parser('a OR b').tokenize()
        ['a ', 'OR', ' b']
        >>> Parser('a OR b AND c').tokenize()
        ['a ', 'OR', ' b ', 'AND', ' c']
        >>> Parser('(a OR b) AND c').tokenize()
        ['(', 'a ', 'OR', ' b', ')', 'AND', ' c']
        >>> Parser('foo bar AND b').tokenize()
        ['foo bar ', 'AND', ' b']
        >>> Parser('(a AND (b OR c)) AND d').tokenize()
        ['(', 'a ', 'AND', '(', 'b ', 'OR', ' c', ')', ')', 'AND', ' d']
        >>> Parser('((a AND b) OR c)').tokenize()
        ['(', '(', 'a ', 'AND', ' b', ')', 'OR', ' c', ')']
        >>> Parser('(a AND ((b1 AND b2 OR b3) OR c)) AND d').tokenize()
        ['(', 'a ', 'AND', '(', '(', 'b1 ', 'AND', ' b2 ', 'OR', ' b3', ')', 'OR', ' c', ')', ')', 'AND', ' d']
        """
        escape = lambda s: s.replace('(', '\(').replace(')', '\)')
        escaped_lexemes = [escape(lexeme) for lexeme in self.LEXEMES]
        regex = '(' + '|'.join(escaped_lexemes) + ')'
        result = re.split(regex, self.expr)
        return filter(lambda x: len(x) > 0 and not x.isspace(), result)

    def validate(self):
        """
        >>> Parser('').validate()
        True
        >>> Parser('a').validate()
        True
        >>> Parser('a = 5').validate()
        True
        >>> Parser('a AND b').validate()
        True
        >>> Parser('a OR b').validate()
        True
        >>> Parser('a AND b AND c').validate()
        True
        >>> Parser('a AND b OR c').validate()
        True
        >>> Parser('a OR b AND c').validate()
        True
        >>> Parser('a OR b OR c').validate()
        True
        >>> Parser('a OR b OR c AND d OR e').validate()
        True
        >>> Parser('(a)').validate()
        True
        >>> Parser('((a))').validate()
        True
        >>> Parser('(a AND b)').validate()
        True
        >>> Parser('(a AND b) AND c').validate()
        True
        >>> Parser('(a AND b) AND (c OR d)').validate()
        True
        >>> Parser('(a AND (b OR c))').validate()
        True
        >>> Parser('((a AND b) OR c)').validate()
        True
        >>> Parser('(a AND (b OR c)) AND d').validate()
        True
        >>> Parser('(a AND ((b1 AND b2 OR b3) OR c)) AND d').validate()
        True
        >>> Parser('(').validate()
        False
        >>> Parser(')').validate()
        False
        >>> Parser('(a').validate()
        False
        >>> Parser(')(').validate()
        False
        >>> Parser('()').validate()
        False
        >>> Parser('a AND ()').validate()
        False
        """
        self.tokens = self.tokenize()
        self.valid = True
        self.next_token()
        self.expression()
        return self.valid

    def next_token(self):
        self.current_token = self.tokens[0] if self.tokens else None
        self.tokens = self.tokens[1:]

    def accept(self, token):
        matches = token is not self.OTHER and self.current_token == token
        other = token is self.OTHER and self.current_token not in self.LEXEMES
        if matches or other:
            self.next_token()
            return True
        return False

    def expect(self, token):
        if not self.accept(token):
            self.valid = False

    def expression(self):
        if self.accept(self.OTHER):
            if self.accept('AND') or self.accept('OR'):
                self.expression()
        elif self.accept('('):
            self.expression()
            self.expect(')')
        else:
            self.valid = False


doctest.testmod()

