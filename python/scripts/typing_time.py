#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Mierzy czas i prędkość przepisywania tekstu oraz ilość przepisanych znaków.

Przepisany tekst można przekierować do pliku.

"""

import codecs
import sys
import time

text = ''
time_started = time.time()
for line in sys.stdin.readlines():
    text += line
    print line,
time_finished = time.time()
typing_time = time_finished - time_started
text = codecs.decode(text, 'UTF-8')
print >> sys.stderr, u'czas pisania:', typing_time, u's'
print >> sys.stderr, u'długość tekstu:', len(text), u'znaków'
print >> sys.stderr, u'prędkość:', len(text) / typing_time * 60, u'znaków/min'
