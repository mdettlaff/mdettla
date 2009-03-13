#!/usr/bin/python
# -*- coding: UTF-8 -*-

# Program przestawiający losowo litery wewnątrz wyrazów.

import sys
import random

def str_shuffle(string):
    """Przestawia losowo litery w podanym napisie."""
    str_list = []
    for c in string:
        str_list.append(c)
    random.shuffle(str_list)
    string = ''
    for c in str_list:
        string += c
    return string

if __name__ == '__main__':
    try:
        text = []

        if len(sys.argv) > 2:
            print 'Użycie: python shuffle.py [PLIK]'
        elif len(sys.argv) == 2:
            file = open(sys.argv[1])
            for line in file:
                text += unicode(line, 'utf-8').split(' ')
        else:
            for line in sys.stdin.readlines():
                text += unicode(line, 'utf-8').split(' ')

        for word in text:
            end = -1
            for i in range(len(word)-1, 1, -1):
                if not word[i].isalpha():
                    end -= 1
            if len(word) > 3:
                word = word[0] + \
                        str_shuffle(word[1:end]) + \
                        word[end:-1] + word[-1]
            print word,
    except IOError:
        print 'Nie znaleziono pliku', sys.argv[1]
        sys.exit(1)

