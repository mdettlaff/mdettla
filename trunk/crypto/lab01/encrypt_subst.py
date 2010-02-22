#!/usr/bin/env python

from string import ascii_uppercase
import random
import sys

permutation = random.sample(ascii_uppercase, len(ascii_uppercase))
plaintext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
print ''.join([permutation[ascii_uppercase.index(c)] for c in plaintext])
