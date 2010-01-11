if [ $# -eq 0 ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  javac -d bin src/mdettla/ea/zadanie2/*.java
else
  java -cp bin mdettla.ea.zadanie2.CompareAlgorithms
fi
