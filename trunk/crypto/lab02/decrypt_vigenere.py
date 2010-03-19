#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from string import ascii_uppercase
import sys

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

key_len = 5 # obliczyć z Kasiskiego

ciphertext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])

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
    print key

