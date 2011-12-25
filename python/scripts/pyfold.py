#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

"""Replacement for "fold -w" that can handle UTF-8 correctly."""

import codecs
import doctest
import re
import sys

DEFAULT_WIDTH = 80
ENCODING = 'UTF-8'


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
    >>> fold('foo        ba', 4)
    'foo \\n    \\n ba'
    >>> fold('fo barbazquux', 4)
    'fo\\nbarb\\nazqu\\nux'
    """
    assert width > 0, 'width must be a positive number'
    islinespace = lambda s: re.match(' +$', s)
    line_len = 0
    tokens = filter(None, re.split('( +|\n)', text))
    i = 0
    while i < len(tokens):
        token = tokens[i]
        if token == '\n':
            line_len = 0
        else:
            line_len += len(token)
            if line_len > width:
                cut = len(token) - (line_len - width)
                if len(token) == line_len or islinespace(token):
                    if islinespace(token):
                        cut += 1
                    tokens[i:i + 1] = [token[:cut], '\n', token[cut:]]
                else:
                    tokens[i:i + 1] = ['', '\n', token]
        i += 1
    return ''.join(tokens).replace(' \n', '\n')


doctest.testmod()

if __name__ == '__main__':
    width = int(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_WIDTH
    input_lines = codecs.decode(''.join(sys.stdin.readlines()), ENCODING)
    print codecs.encode(fold(input_lines, width), ENCODING),

