#!/usr/bin/env python
# -*- encoding: UTF-8 -*-

import doctest
import sys


def number_to_words(number):
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
    >>> print f(13000)
    trzynaście tysięcy
    >>> print f(25000)
    dwadzieścia pięć tysięcy
    >>> print f(32187)
    trzydzieści dwa tysiące sto osiemdziesiąt siedem
    >>> print f(112000)
    sto dwanaście tysięcy
    >>> print f(43064101)
    czterdzieści trzy miliony sześćdziesiąt cztery tysiące sto jeden
    >>> print f(1000000000)
    jeden miliard
    >>> print f(1000001000)
    jeden miliard jeden tysiąc
    >>> print f(125000000000)
    sto dwadzieścia pięć miliardów
    >>> print f(12345002000103)
    12345 miliardów dwa miliony sto trzy
    """

    singles = [None, 'jeden', 'dwa', 'trzy', 'cztery', 'pięć', 'sześć', 'siedem', 'osiem', 'dziewięć']
    teens = ['jedenaście', 'dwanaście', 'trzynaście', 'czternaście', 'piętnaście', 'szesnaście', 'siedemnaście', 'osiemnaście', 'dziewiętnaście']
    tenths = [None, 'dziesięć', 'dwadzieścia', 'trzydzieści', 'czterdzieści', 'pięćdziesiąt', 'sześćdziesiąt', 'siedemdziesiąt', 'osiemdziesiąt', 'dziewięćdziesiąt']
    hundreds = [None, 'sto', 'dwieście', 'trzysta', 'czterysta', 'pięćset', 'sześćset', 'siedemset', 'osiemset', 'dziewięćset']
    thousandfold_multiplier = [
            [None, None, None],
            ['tysiąc', 'tysiące', 'tysięcy'],
            ['milion', 'miliony', 'milionów'],
            ['miliard', 'miliardy', 'miliardów'],
            [None, None, None]]

    def multiplier_form(multiplier, number):
        if number == 0:
            return None
        elif number == 1:
            return multiplier[0]
        elif 2 <= number % 10 < 5 and not 12 <= number % 100 < 15:
            return multiplier[1]
        else:
            return multiplier[2]

    if number == 0:
        return 'zero'
    words = []
    i = 0
    while number > 0:
        teen = teens[number % 100 - 11] if 11 <= number % 100 < 20 else None
        hundredth = hundreds[(number / 100) % 10]
        tenth = tenths[(number / 10) % 10] if not teen else teen
        single = singles[number % 10] if not teen else None
        multiplier = multiplier_form(thousandfold_multiplier[i], number % 1000)
        if i + 2 >= len(thousandfold_multiplier) and number > 1000:
            words = [str(number), multiplier] + words
            break
        words = [hundredth, tenth, single, multiplier] + words
        number /= 1000
        i += 1
    return ' '.join(filter(lambda x: x is not None, words))


doctest.testmod()

if __name__ == '__main__' and len(sys.argv) > 1:
    print number_to_words(int(sys.argv[1]))

