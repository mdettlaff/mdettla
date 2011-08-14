#!/bin/sh

set -u

grep "[0-9] [0-9]\+\.[0-9]\+" > /tmp/fitness_time.dat

gp_file=/tmp/gp.gp
echo 'set terminal png size 640, 480 crop' > $gp_file
echo 'set output "/tmp/fitness_time.png"' >> $gp_file
echo 'set grid' >> $gp_file
echo 'unset label' >> $gp_file
echo 'set xlabel "Liczba iteracji"' >> $gp_file
echo 'set ylabel "Przystosowanie (p)"' >> $gp_file
echo 'plot "/tmp/fitness_time.dat" using 1:2 with lines' >> $gp_file

gnuplot $gp_file
convert /tmp/fitness_time.png fig/$1.jpg
