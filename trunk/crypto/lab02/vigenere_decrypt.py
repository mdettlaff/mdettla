#!/usr/bin/env python

from string import ascii_uppercase
import sys

def vigenere_decrypt(ciphertext, key):
    return ''.join([ascii_uppercase[(ascii_uppercase.index(ciphertext[i]) \
            - ascii_uppercase.index(key[i % len(key)])) \
            % len(ascii_uppercase)] for i in range(len(ciphertext))])

def main(argv):
    ciphertext = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
    key = argv[1]
    print vigenere_decrypt(ciphertext, key)

if __name__ == '__main__':
    main(sys.argv)

