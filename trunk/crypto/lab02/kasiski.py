#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

import sys

pattern_len = int(sys.argv[1])
ciphertext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
dist_occurs = {}
for i in range(len(ciphertext) - pattern_len):
    possible_pattern = ciphertext[i:i + pattern_len]
    for j in range(i + 1, len(ciphertext) - pattern_len + 1):
        if ciphertext[j:j + pattern_len] == possible_pattern:
            distance = j - i
            if distance not in dist_occurs:
                dist_occurs[distance] = 0
            dist_occurs[distance] += 1
len_candidates = {}
for i in range(pattern_len, pattern_len + 10):
    len_candidates[i] = 0
    for dist, occurs in dist_occurs.iteritems():
        if dist % i == 0:
            len_candidates[i] += occurs
print 'długość\tpasujące'
print 'klucza\tprzypadki'
for i in sorted(len_candidates, key = len_candidates.__getitem__, reverse = True):
    if len_candidates[i] > 0:
        print '%d\t%d' % (i, len_candidates[i])

