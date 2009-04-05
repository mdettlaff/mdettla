#!/usr/bin/python
# -*- coding: utf8 -*-

import sys

def mpg2lhk(mpg):
    """Konwersja z mil na galon na litry na 100 kilometrów."""
    return (100 * 4.546) / (mpg * 1.609)

if (len(sys.argv) != 2):
    print "Konwersja z mil na galon na litry na 100 kilometrów."
    print "Użycie:", sys.argv[0], "mpg"
else:
    mpg = float(sys.argv[1])
    print mpg2lhk(mpg)
