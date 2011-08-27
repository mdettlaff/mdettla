#!/bin/bash

GA="java -cp build/classes mdettla.keyboard.ga.GAKeyboard quiet=true"
RESULTS_FILE="results/results.txt"
EN_CORPUS="src/mdettla/keyboard/ga/resources/en/*.txt"

echo -n > $RESULTS_FILE
$GA populationSize=20 generationsCount=50 $EN_CORPUS >> $RESULTS_FILE
echo -e "\n" >> $RESULTS_FILE
$GA populationSize=30 generationsCount=60 $EN_CORPUS >> $RESULTS_FILE
