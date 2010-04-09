#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

from string import ascii_uppercase as CHARS

plaintext = 'RECONNAISSANCE'
ciphertext = 'UPYTEZOJZEGBOT'
#                             ABCDEFGHIJKLMNOPQRSTUVWXYZ
N = [CHARS.index(c) for c in 'EKMFLGDQVZNTOWYHXUSPAIBRCJ']

def rod_char(letter_index, shift, perm):
    return (perm[(letter_index + shift) % len(CHARS)] - shift) % len(CHARS)

def is_involution_valid(involution_str):
    involution_dict = {}
    for i in range(0, len(involution_str), 3):
        char1 = involution_str[i]
        char2 = involution_str[i + 1]
        if char1 in involution_dict and involution_dict[char1] != char2 \
                or char2 in involution_dict and involution_dict[char2] != char1:
            return False
        else:
            involution_dict[char1] = char2
            involution_dict[char2] = char1
    return True

for starting_position in range(len(CHARS)):
    involution_str = ''
    for i in range(len(plaintext)):
        char1 = rod_char(CHARS.index(plaintext[i]), i + starting_position, N)
        char2 = rod_char(CHARS.index(ciphertext[i]), i + starting_position, N)
        involution_str += CHARS[char1] + CHARS[char2] + ' '
    if is_involution_valid(involution_str):
        print u'Pasująca inwolucja:', involution_str
        print u'na pozycji', starting_position

