#!/usr/bin/env python

from string import ascii_lowercase
import sys

def get_chars_freqs(text):
    chars_freqs = {}
    for c in ascii_lowercase:
        chars_freqs[c] = float(text.count(c)) / len(text)
    chars_by_freq = sorted(chars_freqs, key = chars_freqs.get, reverse = True)
    return chars_freqs, chars_by_freq

def get_chars_pairs_freqs(text):
    pairs_occurs = {}
    for c1 in ascii_lowercase:
        for c2 in ascii_lowercase:
            pair = c1 + c2
            pairs_occurs[pair] = text.count(pair)
    all_pairs_occurs = sum(pairs_occurs.values())
    pairs_freqs = {}
    for pair, occurs in pairs_occurs.iteritems():
        pairs_freqs[pair] = float(occurs) / all_pairs_occurs
    pairs_by_freq = sorted(pairs_freqs, key = pairs_freqs.get, reverse = True)
    return pairs_freqs, pairs_by_freq


file_to_str = lambda filename: ''.join(
        [line.strip() for line in open(filename).readlines()]).lower()
text_en = file_to_str(sys.argv[1])
text_pl = file_to_str(sys.argv[2])

chars_en_freqs, chars_en_by_freq = get_chars_freqs(text_en)
chars_pl_freqs, chars_pl_by_freq = get_chars_freqs(text_pl)
pairs_en_freqs, pairs_en_by_freq = get_chars_pairs_freqs(text_en)
pairs_pl_freqs, pairs_pl_by_freq = get_chars_pairs_freqs(text_pl)

format = '%s\t%.2f%%\t%.2f%%'
print 'en pl'
for c in chars_en_by_freq[:5]:
    print format % (c, chars_en_freqs[c] * 100, chars_pl_freqs[c] * 100)
for c in pairs_en_by_freq[:5]:
    print format % (c, pairs_en_freqs[c] * 100, pairs_pl_freqs[c] * 100)
print 'pl en'
for c in chars_pl_by_freq[:5]:
    print format % (c, chars_pl_freqs[c] * 100, chars_en_freqs[c] * 100)
for c in pairs_pl_by_freq[:5]:
    print format % (c, pairs_pl_freqs[c] * 100, pairs_en_freqs[c] * 100)

