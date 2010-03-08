#!/usr/bin/env python

import sys

for word in [word.strip() for word in open('/usr/share/dict/words')]:
    #if len(word) == 7 and word[0].isupper() and len(set(word)) == 7:
    if len(word) == 9 and word[0].isupper() and len(set(word)) == 7 \
            and word[1] == word[6] and word[3] == word[8]:
        print word
