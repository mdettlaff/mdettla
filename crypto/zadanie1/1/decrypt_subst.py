#!/usr/bin/env python

import sys

std_chars_by_freq = open(sys.argv[1]).readline().strip()
encrypted = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
freqs = {}
for c in std_chars_by_freq:
    freqs[c] = float(encrypted.count(c)) / len(encrypted)
chars_by_freq = sorted(freqs, key=freqs.__getitem__)
#chars_by_freq = '      E        CNM PGSHLQA'
chars_by_freq = '    IZEJV R FX CNMBPGSHLQA'
#                ZQXJKVBPYGFWMUCLDRHSNIOATE
#CAHGLMN EAMGPQASG
#LEONARD BERNSTEIN
print ''.join([std_chars_by_freq[chars_by_freq.index(c)] \
        if c in chars_by_freq and c != ' ' else ' ' for c in encrypted])
