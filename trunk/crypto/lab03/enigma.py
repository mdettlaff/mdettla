#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from string import ascii_uppercase as chars
import doctest
import random
import sys


class Enigma:
    u"""Enigma z tylko jednym rotorem."""

    def __init__(self, rotor, reflector):
        self.rotor = rotor
        self.reflector = reflector

    def encrypt(self, input):
        output = ''
        for c in input:
            self.rotor.rotate()
            c = self.rotor.translate_forward(c)
            c = self.reflector.translate(
                    chars[(chars.index(c) - self.rotor.rotation) % len(chars)])
            print 'po odbiciu:', c
            c = chars[(chars.index(c) + self.rotor.rotation) % len(chars)]
            c = self.rotor.translate_backward(c)
            output += c
        return output

class Rotor:
    def __init__(self, permutation):
        self.permutation = permutation
        self.rotation = 0

    def rotate(self):
        self.rotation += 1

    def translate_forward(self, c):
        return self.permutation[(chars.index(c) + self.rotation) % len(chars)]

    def translate_backward(self, c):
        return chars[(self.permutation.index(c) - self.rotation) % len(chars)]

class Reflector:
    def __init__(self, involution):
        self.permutation = involution

    def translate(self, c):
        return self.permutation[chars.index(c)]


def permutation():
    return random.sample(chars, len(chars))

def involution():
    u"""
    >>> invol = involution()
    >>> all([invol[chars.index(invol[chars.index(c)])] == c for c in chars])
    True
    """
    sample = random.sample(chars, len(chars))
    invol = [None for i in range(len(chars))]
    for i in range(0, len(sample), 2):
        invol[chars.index(sample[i])] = sample[i + 1]
        invol[chars.index(sample[i + 1])] = sample[i]
    return invol

def main():
    input = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
    perm = permutation()
    invol = involution()
    print '   ', [c for c in chars]
    print 'N =', perm
    print 'Z =', invol

    enigma = Enigma(Rotor(perm), Reflector(invol))
    encrypted = enigma.encrypt(input)
    print 'po zaszyfrowaniu:', encrypted
    enigma = Enigma(Rotor(perm), Reflector(invol))
    decrypted = enigma.encrypt(encrypted)
    print 'po odszyfrowaniu:', decrypted

if __name__ == '__main__':
    doctest.testmod()
    main()

