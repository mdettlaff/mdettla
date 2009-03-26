#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Wypisuje na wyjście ciąg bajtów podany heksadecymalnie.

Przykład: po podaniu '666f6f' wypisze 'foo'.

"""

import sys


def binary(hex_input):
    u"""Zwraca ciąg bajtów podany heksadecymalnie w napisie hex_input."""
    bytes = ''
    for line in hex_input:
        line = line.rstrip()
        for i in range(0, len(line), 2):
            byte_hex = line[i] + line[i+1]
            byte = chr(int(byte_hex, 16))
            bytes += byte
    return bytes


if __name__ == '__main__':
    try:
        input = sys.stdin.readlines()
        output = binary(input)
        print output,
    except ValueError:
        print u'Błąd: należy podać liczby heksadecymalne'
    except IndexError:
        print u'Błąd: nieparzysta ilość znaków'

