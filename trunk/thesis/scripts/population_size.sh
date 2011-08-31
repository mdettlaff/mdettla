#!/bin/sh

set -e

DIR="$( cd "$( dirname "$0" )" && pwd )"

cp $HOME/workspace/Keyboard/results/population_size.txt /tmp/population_size.dat

gp_file=/tmp/gp.gp
echo 'set terminal png size 640, 320 crop' > $gp_file
echo 'set output "/tmp/population_size.png"' >> $gp_file
echo 'set grid' >> $gp_file
echo 'unset label' >> $gp_file
echo 'set xlabel "Rozmiar populacji"' >> $gp_file
echo 'set ylabel "Przystosowanie (p)"' >> $gp_file
echo 'set xrange [10:150]' >> $gp_file
echo 'set yrange [3:4]' >> $gp_file
echo 'plot "/tmp/population_size.dat" using 1:2 with lines' >> $gp_file

gnuplot $gp_file
convert /tmp/population_size.png $DIR/../fig/population_size.jpg
