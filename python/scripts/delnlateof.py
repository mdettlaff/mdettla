#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Usuń znak nowej linii na końcu podanego pliku."""

import sys

usage = u'Użycie: ' + sys.argv[0] + u' PLIK'

if len(sys.argv) == 2:
    file = open(sys.argv[1], 'r+')
    data = file.read()
    if data.endswith('\n'):
        file.truncate(len(data) - 1)
else:
    print usage

