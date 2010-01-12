if [ $# -eq 0 ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  javac -d bin src/mdettla/ea/zadanie2/*.java
else
  if [ ! -d output ]
  then
    mkdir output
  fi
  echo "Test 1: Funkcja Griewangka"
  echo "Obliczam za pomocą algorytmu Differential Evolution..."
  java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
  mdettla.ea.zadanie2.DifferentialEvolutionGA 1500 \
  mdettla.ea.zadanie2.GriewangkFunction \
  > output/griewangk_de
  echo "Wyniki zapisano do katalogu output"
  echo "Obliczam za pomocą algorytmu Cumulative Step Adaptation..."
  java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
  mdettla.ea.zadanie2.CumulativeStepAdaptationES 1500 \
  mdettla.ea.zadanie2.GriewangkFunction \
  > output/griewangk_csa
  echo "Wyniki zapisano do katalogu output"
  echo
  echo "Test 2: Funkcja Rosenbrocka"
  echo "Obliczam za pomocą algorytmu Differential Evolution..."
  java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
  mdettla.ea.zadanie2.DifferentialEvolutionGA 5000 \
  mdettla.ea.zadanie2.RosenbrockFunction \
  > output/rosenbrock_de
  echo "Wyniki zapisano do katalogu output"
  echo "Obliczam za pomocą algorytmu Cumulative Step Adaptation..."
  java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
  mdettla.ea.zadanie2.CumulativeStepAdaptationES 5000 \
  mdettla.ea.zadanie2.RosenbrockFunction \
  > output/rosenbrock_csa
  echo "Wyniki zapisano do katalogu output"
fi
