#!/usr/bin/env python

import doctest
import re
import sys

BIND_REGEX = '\s+bind => \[(.*)\]'

def bind_parameters(query, query_parameters):
    """
    >>> bind_parameters('select * from foo', [])
    'select * from foo'
    >>> bind_parameters('select * from foo where bar = ?', ['baz'])
    'select * from foo where bar = baz'
    >>> bind_parameters('select * from foo where bar = ? or x = ?', ['baz', 'y'])
    'select * from foo where bar = baz or x = y'
    """
    return query.replace('?', '%s') % tuple(query_parameters)

def get_parameters(bind_line):
    """
    >>> get_parameters('        bind => [2011-11-28, 1970-01-01 00:00:00.0, true, 2, 3.50, BASE, null]')
    ["'2011-11-28'", "'1970-01-01 00:00:00.0'", "'true'", '2', '3.50', "'BASE'", 'null']
    """
    is_number = lambda s: re.match(r'^\d+(\.\d+)?$', s)
    parse_param = lambda p: p if is_number(p) or p == 'null' else "'" + p + "'"
    return [parse_param(p) for p in re.sub(BIND_REGEX, '\\1', bind_line).split(', ')]

def parse_input(input_lines):
    """
    >>> parse_input(['select * from foo\\n'])
    'select * from foo'
    >>> parse_input(['select * from a\\n', 'where b = ? and c < ?\\n', '        bind => [d, 2]\\n'])
    "select * from a\\nwhere b = 'd' and c < 2"
    >>> parse_input(['select a = ?\\n', '        bind => [5]\\n', 'select b = ?\\n', '        bind => [7]\\n'])
    'select a = 5\\nselect b = 7'
    """
    query = ''
    for line in input_lines:
        if re.match(BIND_REGEX, line):
            bind_line = line.rstrip()
            query = bind_parameters(query, get_parameters(bind_line))
        else:
            query += line
    return query.rstrip()

def main():
    print parse_input(sys.stdin.readlines())


doctest.testmod()

if __name__ == '__main__':
    main()

