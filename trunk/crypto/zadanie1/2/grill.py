#!/usr/bin/env python2.6
# -*- encoding: UTF-8 -*-

from itertools import combinations

GRILL = """\
S E I T I R
H T E I N E
C L E S T E
A C I E I W
H T V E E A
E D V E K R\
"""

def empty_grill(size):
    return [[False for c in range(len(grill))] for row in range(len(grill))]

def parse_grill(grill_str):
    return [[c for c in row.split()] for row in GRILL.split('\n')]

def grill_to_str(grill):
    s = ''
    for row in grill:
        for c in row:
            s += (c if c else ' ') + ' '
        s += '\n'
    return s[:-1]

def rotate_grill(grill):
    rotated = empty_grill(len(grill))
    for i in range(len(grill)):
        for j in range(len(grill)):
            rotated[i][(len(grill) - 1) - j] = grill[j][i];
    return rotated

def create_mask(combination, size):
    mask = empty_grill(size)
    for i in range(size):
        for j in range(size):
            if len(combination) > i * size + j and combination[i * size + j]:
                mask[i][j] = True
    return mask

def apply_mask(mask, grill):
    decrypted = ''
    for i in range(len(grill)):
        for j in range(len(grill)):
            if mask[i][j]:
                decrypted += grill[i][j]
    return decrypted

def decrypt(grill):
    COMB_RANGE = 18
    count = 0
    for combination in combinations(range(COMB_RANGE), 6):
        combination = [(i in combination) for i in range(COMB_RANGE)]
        mask = create_mask(combination, len(grill))

        if mask[0][0] and mask[0][5]: continue
        if mask[0][1] and mask[1][5]: continue
        if mask[0][2] and mask[2][5]: continue
        if mask[1][0] and mask[0][4]: continue
        if mask[1][1] and mask[1][4]: continue
        if mask[1][2] and mask[2][4]: continue
        if mask[2][0] and mask[0][3]: continue
        if mask[2][1] and mask[1][3]: continue
        if mask[2][2] and mask[2][3]: continue

        s = ''
        for i in range(4):
            #print grill_to_str(grill)
            s += apply_mask(mask, grill) + ' '
            grill = rotate_grill(grill)
        print s
        count += 1
    print '\n', grill_to_str(grill), '\n'
    print 'ilość powtórzeń:', count

def show_rotations(grill):
    print 'obroty:'
    for i in range(4):
        print grill_to_str(grill)
        print
        grill = rotate_grill(grill)


grill = parse_grill(GRILL)

decrypt(grill)
print
show_rotations(grill)

