#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from string import ascii_uppercase
import doctest
import random
import sys


class Enigma:
    def __init__(self, rotors, reflector):
        self.rotors = rotors
        self.reflector = reflector

    def encrypt(self, input):
        return ''.join([self.__encrypt_char(c) for c in input])

    def __rotate(self, rotors):
        if rotors[0].rotation == len(rotors[0]) - 1 and len(rotors) > 1:
            __rotate(rotors[1:])
        rotors[0].rotate()

    def __encrypt_char(self, c):
        self.__rotate(self.rotors)
        position = ascii_uppercase.index(c)
        for rotor in self.rotors:
            position = rotor.out_position_forward(position)
        position = self.reflector.out_position(position)
        for rotor in reversed(self.rotors):
            position = rotor.out_position_backward(position)
        return ascii_uppercase[position]

class Rotor:
    def __init__(self, permutation, initial_rotation):
        self.permutation = permutation
        self.rotation = initial_rotation

    def rotate(self):
        self.rotation = (self.rotation + 1) % len(self)

    def out_position_forward(self, in_position):
        return (self.permutation[(in_position + self.rotation) \
                % len(self)] - self.rotation) % len(self)

    def out_position_backward(self, in_position):
        return (self.permutation.index((in_position + self.rotation) \
                % len(self)) - self.rotation) % len(self)

    def __len__(self):
        return len(self.permutation)

class Reflector:
    def __init__(self, involution):
            self.permutation = involution

    def out_position(self, in_position):
        return self.permutation[in_position]


def permutation(sequence):
    return random.sample(sequence, len(sequence))

def involution(sequence):
    u"""
    >>> invol = involution(range(len(ascii_uppercase)))
    >>> all([invol[invol[i]] == i for i in range(len(ascii_uppercase))])
    True
    """
    perm = permutation(sequence)
    invol = [None for i in range(len(sequence))]
    for i in range(0, len(perm), 2):
        invol[sequence.index(perm[i])] = perm[i + 1]
        invol[sequence.index(perm[i + 1])] = perm[i]
    return invol

def main():
    input = ''.join([line.strip().upper() for line in sys.stdin.readlines()])
    N = permutation(range(len(ascii_uppercase)))
    Z = involution(range(len(ascii_uppercase)))

    print u'Enigma bez łącznicy NZN^1'
    print 'N =', ''.join([ascii_uppercase[i] for i in N])
    print 'Z =', ''.join([ascii_uppercase[i] for i in Z])

    enigma = Enigma([Rotor(N, 0)], Reflector(Z))
    encrypted = enigma.encrypt(input)
    print u'po zaszyfrowaniu:', encrypted
    enigma = Enigma([Rotor(N, 0)], Reflector(Z))
    decrypted = enigma.encrypt(encrypted)
    print u'po odszyfrowaniu:', decrypted

if __name__ == '__main__':
    doctest.testmod()
    main()

