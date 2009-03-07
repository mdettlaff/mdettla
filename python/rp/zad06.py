#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
#       zad06.py
#       
#       Copyright 2009 Piotr Labudda <piotrl86@gmail.com>
#       
#
# ----------------------------------------------------------------------------
# "THE BEER-WARE LICENSE" (Revision 42):
# <piotrl86@gmail.com> wrote this file. As long as you retain this notice you
# can do whatever you want with this stuff. If we meet some day, and you think
# this stuff is worth it, you can buy me a beer in return Poul-Henning Kamp
# ----------------------------------------------------------------------------
#

import sys, random

if __name__ == '__main__' :
  if len(sys.argv) != 2 :
    print 'Użycie: %s NUMER_ZADANIA' % (sys.argv[0], )
    sys.exit(1)

  numer = sys.argv[1] # numer zadania (str)

  fz = open('zadania.txt')
  pr = []
  m = 0
  readPr = False
  for line in fz.readlines() :
    if readPr == True :
      if line[0:3] == 'm =' :
        m = int(line[4:])
        break
      else :
        pr.append(float(line))
    elif line[0:6] == 'Zestaw' :
      dane = line[:-1].split(':', 1)
      if dane[1] == numer :
        readPr = True
  fz.close()

  if m == 0 :
    print 'Error.'
    sys.exit(1)

  print 'm = '+str(m)
  print 'pr:'
  for i in range(len(pr)) :
    suma = pr[i]
    if i % 3 > 0 :
      suma = suma + pr[i - 1]
      if i % 3 > 1 :
        suma = suma + pr[i - 2]
    print '%d %.4f %.6f => %d' % (int(i / 3), pr[i], suma, i % 3)

  """
  print random.random()
  This prints a random floating point number in the range [0, 1)
  (that is, between 0 and 1, including 0.0 but always smaller than 1.0).
  """

  fout = open('ciag.txt', 'w')
  wierzcholek = 0 # E_1 (0,1,2)
  #print 'Start:'
  #print 'E_%d' % (wierzcholek, )
  odp1 = ''
  odp2 = [0, 0, 0]
  odp3, odp3LicznikStop = 0, False
  # ś(x)=x, ś(x+y)=(ś(x)*1+y)/2, ś(x+y+z)=(ś(x+y)+z)/3
  odp4, odp4Ile, odp4Kroki = 0.0, 0, 0
  fout.write(str(wierzcholek + 1))
  for i in range(m-1) :
    rnd = random.random()
    indx = wierzcholek*3
    if rnd < pr[indx] :
      wierzcholek = 0
    elif rnd < pr[indx] + pr[indx + 1] :
      wierzcholek = 1
    else :
      wierzcholek = 2
    # odpowiedz 1
    if i > m - 102 :
      odp1 = odp1 + str(wierzcholek + 1)
      if (i - m + 102) % 25 == 0 :
        odp1 = odp1 + ','
    # odpowiedz 2
    odp2[wierzcholek] = odp2[wierzcholek] + 1
    # odpowiedz 3
    if odp3LicznikStop == False :
      odp3 = odp3 + 1
      if wierzcholek == 0 :
        odp3LicznikStop = True
    # odpoweidz 4
    odp4Kroki = odp4Kroki + 1
    if wierzcholek == 0 :
      odp4 = (odp4 * odp4Ile + odp4Kroki) / (odp4Ile + 1)
      odp4Ile = odp4Ile + 1
      odp4Kroki = 0
    fout.write(str(wierzcholek + 1))
    #print 'r = %.6f => _%d' % (rnd, wierzcholek)
    #raw_input()
  fout.close()

  print '\nodp 1 :'
  for i in odp1.split(',') :
    print i
  print '\nodp 2 :'
  # m    - 100%
  # odp2 -   x%
  print 'E_1 : %.4f' % (float(odp2[0]) / m, )
  print 'E_2 : %.4f' % (float(odp2[1]) / m, )
  print 'E_3 : %.4f' % (float(odp2[2]) / m, )
  print '\nodp 3 :'
  print 'pierwszy raz wrócił do E_1 po %d krokach' % (odp3, )
  print '\nodp 4 :'
  print 'śr liczba kroków po których łańcuch powraca do E_1 : %.4f' % (odp4, )
