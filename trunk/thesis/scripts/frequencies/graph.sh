#!/bin/sh

set -u

DIR="$( cd "$( dirname "$0" )" && pwd )"

$DIR/frequencies.py \
  /home/mdettla/workspace/Keyboard/src/mdettla/keyboard/ga/resources/en/otoos11.txt \
  /home/mdettla/workspace/Keyboard/src/mdettla/keyboard/ga/resources/pl/pg8119.txt \
  > /tmp/data_frequencies.dat
cat /tmp/data_frequencies.dat \
  | grep "[a-z] [0-9]\+\.[0-9]\+ [0-9]\+\.[0-9]\+" > /tmp/data.dat

gp_file=/tmp/gp.gp
echo 'set terminal png size 640, 320 crop' > $gp_file
echo 'set output "/tmp/chars_freqs.png"' >> $gp_file
echo 'set grid' >> $gp_file
echo 'unset label' >> $gp_file
echo 'set xlabel "Znak"' >> $gp_file
echo 'set ylabel "Czestotliwosc wystepowania (%)"' >> $gp_file
echo 'set style data histograms' >> $gp_file
echo 'set style fill solid 1.0 border -1' >> $gp_file
echo 'plot "/tmp/data.dat" using 2:xtic(1),\\' >> $gp_file
echo '     "/tmp/data.dat" using 3:xtic(1)' >> $gp_file

gnuplot $gp_file
convert /tmp/chars_freqs.png $DIR/../../fig/frequencies.jpg
