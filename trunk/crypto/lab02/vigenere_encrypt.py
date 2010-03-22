#!/usr/bin/env python

from string import ascii_uppercase
import sys

plaintext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
print ''.join([ascii_uppercase[(ascii_uppercase.index(plaintext[i]) \
        + ascii_uppercase.index(sys.argv[1][i % len(sys.argv[1])])) \
        % len(ascii_uppercase)] for i in range(len(plaintext))])
