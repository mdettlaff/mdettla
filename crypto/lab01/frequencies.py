#!/usr/bin/env python

from string import ascii_uppercase
import sys

std_freqs = [float(line.strip()) / 100 for line in open(sys.argv[1])]
plaintext = ''.join([line.strip() for line in sys.stdin.readlines()])
occur_counts = [float(plaintext.count(ascii_uppercase[i])) \
        for i in range(len(ascii_uppercase))]
freqs = sorted([occur_count / len(plaintext) for occur_count in occur_counts])
d = sum([(std_freqs[i] - freqs[i])**2 for i in range(0, len(std_freqs))])

print 'd =', d
