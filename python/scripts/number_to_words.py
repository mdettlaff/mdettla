#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

import doctest
import sys


def number_to_words(n):
    """
    >>> f = number_to_words
    >>> print f(0), f(1), f(3), f(7), f(9)
    zero jeden trzy siedem dziewięć
    >>> print f(10), f(11), f(13), f(17), f(19)
    dziesięć jedenaście trzynaście siedemnaście dziewiętnaście
    >>> print f(23)
    dwadzieścia trzy
    >>> print f(52)
    pięćdziesiąt dwa
    >>> print f(111)
    sto jedenaście
    >>> print f(187)
    sto osiemdziesiąt siedem
    >>> print f(1000)
    jeden tysiąc
    >>> print f(-2800)
    minus dwa tysiące osiemset
    >>> print f(13000)
    trzynaście tysięcy
    >>> print f(25000)
    dwadzieścia pięć tysięcy
    >>> print f(32187)
    trzydzieści dwa tysiące sto osiemdziesiąt siedem
    >>> print f(112000)
    sto dwanaście tysięcy
    >>> print f(1401203)
    jeden milion czterysta jeden tysięcy dwieście trzy
    >>> print f(16451309)
    szesnaście milionów czterysta pięćdziesiąt jeden tysięcy trzysta dziewięć
    >>> print f(43064101)
    czterdzieści trzy miliony sześćdziesiąt cztery tysiące sto jeden
    >>> print f(1000000000)
    jeden miliard
    >>> print f(1000001000)
    jeden miliard jeden tysiąc
    >>> print f(125000000000)
    sto dwadzieścia pięć miliardów
    >>> print f(1000000000000000000)
    jeden trylion
    >>> print f(5000000000000000000)
    pięć trylionów
    >>> print f(999000000000000000000)
    dziewięćset dziewięćdziesiąt dziewięć trylionów
    >>> print f(1000000000000000000000)
    jeden tysiąc trylionów
    >>> print f(1001000000000000000000)
    jeden tysiąc jeden trylionów
    >>> print f(12345000000000002000103)
    dwanaście tysięcy trzysta czterdzieści pięć trylionów dwa miliony sto trzy
    >>> print f(1000000000000000000000000)
    jeden milion trylionów
    >>> print f(1000000000000000000000000000000000000000000000000000000)
    jeden trylion trylionów trylionów
    """

    times1s = [None] + 'jeden dwa trzy cztery pięć sześć siedem osiem dziewięć'.split(' ')
    teens = 'jedenaście dwanaście trzynaście czternaście piętnaście szesnaście siedemnaście osiemnaście dziewiętnaście'.split(' ')
    times10s = [None] + 'dziesięć dwadzieścia trzydzieści czterdzieści pięćdziesiąt sześćdziesiąt siedemdziesiąt osiemdziesiąt dziewięćdziesiąt'.split(' ')
    times100s = [None] + 'sto dwieście trzysta czterysta pięćset sześćset siedemset osiemset dziewięćset'.split(' ')
    times1000s = [
            [None, None, None],
            ['tysiąc', 'tysiące', 'tysięcy'],
            ['milion', 'miliony', 'milionów'],
            ['miliard', 'miliardy', 'miliardów'],
            ['bilion', 'biliony', 'bilionów'],
            ['biliard', 'biliardy', 'biliardów'],
            ['trylion', 'tryliony', 'trylionów']]

    def get_times1000(n, i):
        is_2nd_form = 2 <= n % 10 < 5 and not 12 <= n % 100 < 15
        return times1000s[i][0 if n == 1 else (1 if is_2nd_form else 2)]

    if n == 0:
        return 'zero'
    elif n < 0:
        return 'minus ' + number_to_words(abs(n))
    words = []
    i = 0
    while n > 0:
        teen = teens[n % 100 - 11] if 11 <= n % 100 < 20 else None
        times100 = times100s[(n / 100) % 10]
        times10 = times10s[(n / 10) % 10] if not teen else teen
        times1 = times1s[n % 10] if not teen else None
        if i >= len(times1000s) - 1:
            words = [number_to_words(n), get_times1000(n, i)] + words
            break
        times1000 = None if n % 1000 == 0 else get_times1000(n % 1000, i)
        words = [times100, times10, times1, times1000] + words
        n /= 1000
        i += 1
    return ' '.join(filter(lambda word: word is not None, words))


doctest.testmod()

if __name__ == '__main__' and len(sys.argv) > 1:
    print number_to_words(int(sys.argv[1]))

