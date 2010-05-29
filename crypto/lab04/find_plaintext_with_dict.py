#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

import sys

plaintext_words = {}
for plaintext_candidate in sys.stdin.readlines():
    words_found_count = 0
    plaintext_candidate = plaintext_candidate.strip().upper()
    for word in open('/usr/share/dict/words').readlines():
        word = word.strip().upper()
        words_found_count += plaintext_candidate.count(word)
    plaintext_words[plaintext_candidate] = words_found_count

best_match = sorted(
        plaintext_words, key = plaintext_words.__getitem__, reverse = True)[0]
print u'najlepszy kandydat na plaintext:'
print best_match
