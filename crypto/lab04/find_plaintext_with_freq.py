#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from string import ascii_uppercase
import sys

std_freqs = [float(line.strip()) / 100 for line in open(sys.argv[1])]
plaintext_ds = {}
for plaintext in sys.stdin.readlines():
    plaintext = plaintext.strip()#.upper()
    occur_counts = [float(plaintext.count(ascii_uppercase[i])) \
            for i in range(len(ascii_uppercase))]
    freqs = [occur_count / len(plaintext) for occur_count in occur_counts]
    d = sum([(std_freqs[i] - freqs[i])**2 for i in range(0, len(std_freqs))])
    plaintext_ds[plaintext] = d
    print 'd(' + plaintext + ') = ', d

best_match = min(plaintext_ds, key = plaintext_ds.__getitem__)
print u'najlepszy kandydat na plaintext:'
print best_match, '(d = ' + str(plaintext_ds[best_match]) + ')'
