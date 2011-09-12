#!/bin/sh

set -u

DIR="$( cd "$( dirname "$0" )" && pwd )"

echo > /tmp/english.txt
for file in $HOME/workspace/Keyboard/src/mdettla/keyboard/ga/resources/en/*.txt
do
  cat $file >> /tmp/english.txt
done
echo > /tmp/polish.txt
for file in $HOME/workspace/Keyboard/src/mdettla/keyboard/ga/resources/pl/*.txt
do
  cat $file >> /tmp/polish.txt
done
$DIR/frequencies.py /tmp/english.txt /tmp/polish.txt > /tmp/data_frequencies.dat
cat /tmp/data_frequencies.dat \
  | grep "[a-z] [0-9]\+\.[0-9]\+ [0-9]\+\.[0-9]\+" > /tmp/data.dat

gp_file=/tmp/gp.gp
echo 'set terminal png size 640, 320 crop' > $gp_file
echo 'set output "/tmp/chars_freqs.png"' >> $gp_file
echo 'set grid' >> $gp_file
echo 'unset label' >> $gp_file
echo 'set ylabel "Czestotliwosc wystepowania (%)"' >> $gp_file
echo 'set style data histograms' >> $gp_file
echo 'set style fill solid 1.0 border -1' >> $gp_file
echo 'plot "/tmp/data.dat" using 2:xtic(1) ti "j. polski",\\' >> $gp_file
echo '     "/tmp/data.dat" using 3:xtic(1) ti "j. angielski"' >> $gp_file

gnuplot $gp_file
convert /tmp/chars_freqs.png $DIR/../../fig/frequencies.jpg
