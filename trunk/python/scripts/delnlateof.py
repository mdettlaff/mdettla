#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Usuń znak nowej linii na końcu podanego pliku."""

import os
import sys

usage = u'Użycie: ' + sys.argv[0] + u' PLIK...'

def main(argv):
    if len(argv) > 1:
        for filename in argv[1:]:
            if os.path.isdir(filename):
                print u'błąd:', filename, u'jest katalogiem'
                continue
            file = open(filename, 'rb+')
            data = file.read()
            if '\0' in data:
                print u'błąd:', filename, u'jest plikiem binarnym'
                continue
            if data.endswith('\n') and not data.endswith('\r\n'):
                file.truncate(len(data) - 1)
            file.close()
    else:
        print usage

if __name__ == '__main__':
    main(sys.argv)

