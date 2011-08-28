#!/bin/bash

set -e -u

GA="java -cp build/classes mdettla.keyboard.ga.GAKeyboard quiet=true"
RESULTS_FILE="results/results.txt"
EN_CORPUS="src/mdettla/keyboard/ga/resources/en/*.txt"

function epochs {
  for i in `seq 4`
  do
    echo $@
    $GA $@ $EN_CORPUS | grep 'przystosowanie:'
  done
  echo
}

epochs mutationOperator=mdettla.jga.operators.mutation.SwapMutation
epochs mutationOperator=mdettla.jga.operators.mutation.MultipleSwapMutation
epochs mutationOperator=mdettla.jga.operators.mutation.DisplacementMutation
epochs mutationOperator=mdettla.jga.operators.mutation.InsertionMutation
epochs mutationOperator=mdettla.jga.operators.mutation.InversionMutation
epochs mutationOperator=mdettla.jga.operators.mutation.ScrambleMutation
epochs mutationOperator=mdettla.jga.operators.mutation.SimpleInversionMutation
