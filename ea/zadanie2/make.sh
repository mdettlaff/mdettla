if [ $# -eq 0 ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  javac -d bin src/mdettla/ea/zadanie2/*.java
else
  if [ "$1" == "doc" ]
  then
    if [ -f output/griewank_de ]
    then
      echo "tworzenie wykresów..."
      cd output
      gnuplot < evo.plt
      cd ..
      mv output/*.png doc
      echo "tworzenie pliku PDF..."
      cd doc
      pdflatex ga2wyniki.tex > /dev/null
      cd ..
      rm doc/ga2wyniki.aux doc/ga2wyniki.log
      echo "zapisano do pliku doc/ga2wyniki.pdf"
    else
      echo "błąd: brak danych do wykresów"
      echo "najpierw uruchom program (polecenie \"./make run\")"
    fi
  else
    if [ ! -d output ]
    then
      mkdir output
    fi
    echo "Test 1: Funkcja Griewanka"
    echo "Obliczam za pomocą algorytmu Differential Evolution"
    echo -n "Proszę czekać"
    java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
    mdettla.ea.zadanie2.DifferentialEvolutionGA 1000 \
    mdettla.ea.zadanie2.GriewankFunction \
    > output/griewank_de
    echo "Wyniki zapisano do katalogu output"
    echo "Obliczam za pomocą algorytmu Cumulative Step Adaptation"
    echo -n "Proszę czekać"
    java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
    mdettla.ea.zadanie2.CumulativeStepAdaptationES 1000 \
    mdettla.ea.zadanie2.GriewankFunction \
    > output/griewank_csa
    echo "Wyniki zapisano do katalogu output"
    echo
    echo "Test 2: Funkcja Rosenbrocka"
    echo "Obliczam za pomocą algorytmu Differential Evolution"
    echo -n "Proszę czekać"
    java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
    mdettla.ea.zadanie2.DifferentialEvolutionGA 500 \
    mdettla.ea.zadanie2.RosenbrockFunction \
    > output/rosenbrock_de
    echo "Wyniki zapisano do katalogu output"
    echo "Obliczam za pomocą algorytmu Cumulative Step Adaptation"
    echo -n "Proszę czekać"
    java -cp bin mdettla.ea.zadanie2.RunEvolutionaryAlgorithm \
    mdettla.ea.zadanie2.CumulativeStepAdaptationES 500 \
    mdettla.ea.zadanie2.RosenbrockFunction \
    > output/rosenbrock_csa
    echo "Wyniki zapisano do katalogu output"
  fi
fi
