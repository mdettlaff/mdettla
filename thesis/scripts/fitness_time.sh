#!/bin/bash

function graph {
  DIR="$( cd "$( dirname "$0" )" && pwd )"
  cat $2 | grep "average: [0-9]\+ [0-9]\+\.[0-9]\+" | sed "s/average: //" > /tmp/fitness_time.dat
  gp_file=/tmp/gp.gp
  echo 'set terminal png size 640, 350 crop' > $gp_file
  echo 'set output "/tmp/fitness_time.png"' >> $gp_file
  echo 'set grid' >> $gp_file
  echo 'unset label' >> $gp_file
  echo 'set xlabel "Liczba iteracji"' >> $gp_file
  echo 'set ylabel "Przystosowanie (p)"' >> $gp_file
  echo 'plot "/tmp/fitness_time.dat" using 1:2 with lines ti "p"' >> $gp_file
  gnuplot $gp_file
  convert /tmp/fitness_time.png $DIR/../fig/$1.jpg
}

function graph_std_dev {
  DIR="$( cd "$( dirname "$0" )" && pwd )"
  cat $2 | grep "std dev: [0-9]\+ [0-9]\+\.[0-9]\+" | sed "s/std dev: //" > /tmp/fitness_time.dat
  gp_file=/tmp/gp.gp
  echo 'set terminal png size 640, 300 crop' > $gp_file
  echo 'set output "/tmp/fitness_time.png"' >> $gp_file
  echo 'set grid' >> $gp_file
  echo 'unset label' >> $gp_file
  echo 'set xlabel "Liczba iteracji"' >> $gp_file
  echo 'set ylabel "Odchylenie standardowe dla p"' >> $gp_file
  echo 'plot "/tmp/fitness_time.dat" using 1:2 with lines ti "odchylenie standardowe dla p"' >> $gp_file
  gnuplot $gp_file
  convert /tmp/fitness_time.png $DIR/../fig/$1.jpg
}

graph fitness_time_en $HOME/workspace/Keyboard/results/results_epochs_en.txt
graph_std_dev std_dev_time_en $HOME/workspace/Keyboard/results/results_epochs_en.txt
graph fitness_time_pl $HOME/workspace/Keyboard/results/results_epochs_pl.txt
graph_std_dev std_dev_time_pl $HOME/workspace/Keyboard/results/results_epochs_pl.txt
