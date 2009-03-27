#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Algorytm αβ-odcinania na przykładzie prostej gry.

Zasady gry:
Gracze na zmianę biorą 1, 2 lub 3 zapałki z n zapałek. Przegrywa ten, kto
weźmie ostatnią zapałkę.

"""


INFINITY = 65535
MIN_TAKE = 1 # ile co najmniej zapałek trzeba zabrać
MAX_TAKE = 3 # ile co najwyżej zapałek można zabrać


def main():
    n = 6

    print u'Możesz wybrać 1, 2 lub 3 zapałki.'
    print u'Przegrywa ten, kto weźmie ostatnią zapałkę.\n'

    while True:
        # gracz
        print u'Pozostało', n, 'zapałek'
        print u'Wybierz ilość zapałek:',
        k = raw_input()
        if 'q' in k:
            break
        k = int(k)
        k = max(min(k, MAX_TAKE, n), MIN_TAKE) # ilość zapałek
        n -= k
        if n == 0:
            print u'Przegrałeś!'
            break

        # komputer
        result = minimax(n, 0)
        k = result[1]
        n -= k
        print u'Pozostało', n, 'zapałek'
        print result[0]
        print u'Komputer wybrał', k, 'zapałek'
        if n == 0:
            print u'Wygrałeś!'
            break


def minimax(n, depth):
    u"""Algorytm Minimax.

    n - ilość zapałek, jakie pozostały

    """
    player_id = depth % 2 # numer gracza: 0 to komputer, 1 to człowiek
    print 'Minimax:', depth, n, player_id
    if n == 0: # jesteśmy w liściu
        return (player_id, 0)
    elif player_id == 0:
        alpha = INFINITY
        k = MIN_TAKE
        for i in range(MIN_TAKE, min(MAX_TAKE+1, n+1)): # k - ilość zapałek
            #alpha = min(alpha, minimax(n-k, depth + 1)[0])
            a = minimax(n-i, depth + 1)[0]
            if a < alpha:
                alpha = a
                k = i
        return (alpha, k)
    elif player_id == 1:
        alpha = -INFINITY
        k = MIN_TAKE
        for i in range(MIN_TAKE, min(MAX_TAKE+1, n+1)): # k - ilość zapałek
            #alpha = max(alpha, minimax(n-k, depth + 1)[0])
            a = minimax(n-i, depth + 1)[0]
            if a > alpha:
                alpha = a
                k = i
        return (alpha, k)


if __name__ == '__main__':
    main()

