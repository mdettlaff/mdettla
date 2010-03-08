#!/usr/bin/env python

from string import ascii_uppercase
import sys

shift = int(sys.argv[1])
encrypted = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
print ''.join([ascii_uppercase[(ascii_uppercase.index(c) + shift) % \
    len(ascii_uppercase)] if c in ascii_uppercase else c for c in encrypted])
