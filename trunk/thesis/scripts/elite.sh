#!/bin/sh

set -u

DIR="$( cd "$( dirname "$0" )" && pwd )"

elite_sizes="0 2 4"
for elite_size in $elite_sizes; do
  cat $HOME/workspace/Keyboard/results/results_elite_$elite_size.txt \
    | grep "[0-9] [0-9]\+\.[0-9]\+" > /tmp/elite_$elite_size.dat
done

gp_file=/tmp/gp.gp
echo 'set terminal png size 640, 320 crop' > $gp_file
echo 'set output "/tmp/graph.png"' >> $gp_file
echo 'set grid' >> $gp_file
echo 'unset label' >> $gp_file
echo 'set xlabel "Liczba iteracji"' >> $gp_file
echo 'set ylabel "Przystosowanie (p)"' >> $gp_file
echo 'set xrange [0:200]' >> $gp_file
echo 'plot \\' >> $gp_file
i=0
for elite_size in $elite_sizes; do
  i=`expr $i + 1`
  array_len=`echo $elite_sizes | wc -w`
  if [ "$i" != "$array_len" ]
  then
    echo '     "/tmp/elite_'$elite_size'.dat" using 1:2 with lines ti "rozmiar elity = '$elite_size'",\\' >> $gp_file
  else
    echo '     "/tmp/elite_'$elite_size'.dat" using 1:2 with lines ti "rozmiar elity = '$elite_size'"' >> $gp_file
  fi
done

gnuplot $gp_file
convert /tmp/graph.png $DIR/../fig/elite.jpg
