#!/usr/bin/env python
# -*- coding: UTF-8 -*-

u"""Algorytm Alfa-Beta na przykładzie prostej gry.
<p>
Zasady gry:<br>
Gracze na zmianę biorą 1, 2 lub 3 zapałki z n zapałek. Przegrywa ten, kto
weźmie ostatnią zapałkę.

"""

__docformat__ = 'javadoc'

import sys
import getopt


usage = u"""\
Użycie: python alpha_beta.py [opcje]
Opcje:
        -n ILOŚĆ_ZAPAŁEK  Rozpocznij grę z podaną ilością zapałek.\
"""

INFINITY = float('Infinity')
MIN_TAKE = 1
u"""Ile co najmniej zapałek trzeba zabrać."""
MAX_TAKE = 3
u"""Ile co najwyżej zapałek można zabrać."""


def main():
    n = 10 # ilość zapałek na początku gry
    try:
        options, args = getopt.getopt(sys.argv[1:], 'n:h', ['help'])
    except getopt.GetoptError, error:
        print str(error)
        print usage
        sys.exit(2)
    for option, argument in options:
        if option in ('-h', '--help'):
            print usage
            sys.exit()
        elif option == '-n':
            n = int(argument)

    print u'Możesz wybrać 1, 2 lub 3 zapałki.'
    print u'Przegrywa ten, kto weźmie ostatnią zapałkę.\n'
    while True:
        # gracz
        print u'Pozostało', n, u'zapałek'
        print u'Wybierz ilość zapałek:',
        decision = raw_input()
        if 'q' in decision:
            break
        decision = int(decision)
        decision = max(min(decision, MAX_TAKE, n), MIN_TAKE) # ilość zapałek
        n -= decision
        if n == 0:
            print u'Przegrałeś!'
            break
        # komputer
        decision = alphabeta(n, 0, -INFINITY, +INFINITY)[1]
        n -= decision
        print u'Komputer wybrał', decision, u'zapałek'
        if n == 0:
            print u'Wygrałeś!'
            break


def alphabeta(n, player_id, alpha, beta):
    u"""Algorytm Alfa-Beta.

    @param n          Ilość zapałek, jakie pozostały.
    @param player_id  Numer gracza: 0 to komputer, 1 to człowiek.
    @param alpha      Wartość węzła.
    @param beta       Najlepszy wybór poprzedniego gracza.

    @return <li>Wartość węzła.
            <li>Ile wziąć zapałek.

    """
    if n == 0: # jesteśmy w liściu
        return player_id, None
    decision = MIN_TAKE # ile wziąć zapałek
    for i in range(MIN_TAKE, min(MAX_TAKE+1, n+1)): # i - ilość zapałek
        a = -alphabeta(n-i, 1 - player_id, -beta, -alpha)[0]
        if a > alpha:
            alpha = a
            decision = i
        if beta <= alpha: # beta-odcięcie
            break
    return alpha, decision


if __name__ == '__main__':
    main()

