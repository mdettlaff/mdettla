#!/bin/bash

set -e -u

GA="java -cp build/classes mdettla.keyboard.ga.GAKeyboard quiet=true"
RESULTS_FILE="results/results.txt"
EN_CORPUS="src/mdettla/keyboard/ga/resources/en/*.txt"

function epochs {
  count=5
  sum=0
  for i in `seq $count`
  do
    echo $@
    fitness=`$GA $@ $EN_CORPUS | grep 'przystosowanie:' | sed 's/przystosowanie: //'`
    echo $fitness
    sum=$(echo "scale=3; $sum + $fitness" | bc)
  done
  echo średnia: $(echo "scale=3; $sum / $count" | bc)
  echo
}

#epochs crossoverProbability=0
#epochs mutationProbability=0

epochs crossoverOperator=mdettla.jga.operators.crossover.CycleCrossover
epochs crossoverOperator=mdettla.jga.operators.crossover.PartiallyMappedCrossover
epochs crossoverOperator=mdettla.jga.operators.crossover.PositionBasedCrossover
epochs crossoverOperator=mdettla.jga.operators.crossover.AlternatingPositionCrossover

epochs mutationOperator=mdettla.jga.operators.mutation.SwapMutation
epochs mutationOperator=mdettla.jga.operators.mutation.MultipleSwapMutation
epochs mutationOperator=mdettla.jga.operators.mutation.DisplacementMutation
epochs mutationOperator=mdettla.jga.operators.mutation.InsertionMutation
epochs mutationOperator=mdettla.jga.operators.mutation.InversionMutation
epochs mutationOperator=mdettla.jga.operators.mutation.ScrambleMutation
epochs mutationOperator=mdettla.jga.operators.mutation.SimpleInversionMutation
