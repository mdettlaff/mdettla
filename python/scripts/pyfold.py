#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

"""Replacement for "fold -w" that can handle UTF-8 correctly."""

import doctest
import re
import sys


def fold(text, width):
    """
    >>> fold('', 80)
    ''
    >>> fold(' ', 80)
    ' '
    >>> fold('foo', 80)
    'foo'
    >>> fold(' foo ', 80)
    ' foo '
    >>> fold('foo bar', 4)
    'foo\\nbar'
    >>> fold('foo  bar', 4)
    'foo \\nbar'
    >>> fold('fo  b', 4)
    'fo \\nb'
    >>> fold('foo bar', 5)
    'foo\\nbar'
    >>> fold('foo bar', 6)
    'foo\\nbar'
    >>> fold('foo bar', 7)
    'foo bar'
    >>> fold('foo bar', 80)
    'foo bar'
    >>> fold('foo bar baz', 10)
    'foo bar\\nbaz'
    >>> fold('foo bar baz qux quux', 10)
    'foo bar\\nbaz qux\\nquux'
    >>> fold('foobar', 3)
    'foo\\nbar'
    >>> fold('foobarbazquux', 3)
    'foo\\nbar\\nbaz\\nquu\\nx'
    >>> fold('foobar b az', 4)
    'foob\\nar b\\naz'
    >>> fold('foo\\n', 80)
    'foo\\n'
    >>> fold('foo\\n\\n\\nbar b qux', 5)
    'foo\\n\\n\\nbar b\\nqux'
    """
    LINESPACE = '[ \t]+'
    islinespace = lambda s: re.match(LINESPACE + '$', s)
    line_len = 0
    tokens = filter(None, re.split('(' + LINESPACE + '|\n)', text))
    i = 0
    while i < len(tokens):
        token = tokens[i]
        if token == '\n':
            line_len = 0
        else:
            line_len += len(token)
            next_token_len = len(tokens[i + 1]) if i + 1 < len(tokens) else 0
            if len(token) > width:
                tokens[i:i + 1] = [token[:width], '\n', token[width:]]
            elif islinespace(token) and line_len + next_token_len > width:
                tokens[i:i + 1] = [token[:-1], '\n']
        i += 1
    return ''.join(tokens)


doctest.testmod()

if __name__ == '__main__':
    width = int(sys.argv[1]) if len(sys.argv) > 1 else 80
    input_lines = ''.join(sys.stdin.readlines())
    print fold(input_lines, width),

