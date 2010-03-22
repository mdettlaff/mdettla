#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from vigenere_decrypt import vigenere_decrypt
from string import ascii_uppercase
import sys

def kasiski(ciphertext, pattern_len = 4):
    dist_occurs = {}
    for i in range(len(ciphertext) - pattern_len):
        pattern = ciphertext[i:i + pattern_len]
        for j in range(i + 1, len(ciphertext) - pattern_len + 1):
            if ciphertext[j:j + pattern_len] == pattern:
                distance = j - i
                if distance not in dist_occurs:
                    dist_occurs[distance] = 0
                dist_occurs[distance] += 1
                break
    len_candidates = {}
    for i in range(pattern_len, pattern_len + 10):
        len_candidates[i] = 0
        for dist, occurs in dist_occurs.iteritems():
            if dist % i == 0:
                len_candidates[i] += occurs
    print 'długość\tpasujące'
    print 'klucza\tprzypadki'
    lengths = sorted(len_candidates, key = len_candidates.__getitem__, reverse = True)
    for i in lengths:
        if len_candidates[i] > 0:
            print '%d\t%d' % (i, len_candidates[i])
    return lengths[0]

def caesar_shift(plaintext, shift):
    return ''.join([ascii_uppercase[(ascii_uppercase.index(c) + shift) % \
            len(ascii_uppercase)] if c in ascii_uppercase else c \
            for c in plaintext])

def subsequence(sequence, start, span):
    return [sequence[i] for i in range(start, len(sequence), span)]

def MIC(x, y):
    MIC_avg = lambda z, j: z.count(ascii_uppercase[j]) / float(len(z))
    return sum([MIC_avg(x, i) * MIC_avg(y, i) \
            for i in range(len(ascii_uppercase))])

ciphertext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])

print 'Szukanie długości klucza metodą Kasiskiego...'
key_len = kasiski(ciphertext)
print 'Znaleziona długość klucza:', key_len

best_matching_shifts = []
for key_shift in range(1, key_len):
    best_matching_shift = 0
    max_MIC = 0
    for shift in range(0, 25):
        current_MIC = MIC(
                caesar_shift(subsequence(ciphertext, 0, key_len), 0),
                caesar_shift(subsequence(ciphertext, key_shift, key_len), shift))
        if current_MIC > max_MIC:
            max_MIC = current_MIC
            best_matching_shift = shift
    best_matching_shifts.append(best_matching_shift)
print 'Przesunięcia:'
print best_matching_shifts

print 'Możliwe klucze:'
for c in ascii_uppercase:
    key = c
    for shift in best_matching_shifts:
        key += ascii_uppercase[ascii_uppercase.index(c) - shift]
    print key, '->', vigenere_decrypt(ciphertext[:50], key)

