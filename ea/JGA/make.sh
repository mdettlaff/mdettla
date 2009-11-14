if [ $# -eq 0 ]
then
  javac -d bin -sourcepath src -cp lib/junit.jar \
    src/mdettla/jga/core/*.java \
    src/mdettla/jga/operators/mutation/*.java \
    src/mdettla/jga/operators/crossover/*.java \
    src/mdettla/jga/operators/selection/*.java \
    src/mdettla/jga/test/*.java
else
  java -cp bin:lib/junit.jar \
  org.junit.runner.JUnitCore \
  mdettla.jga.test.GeneticAlgorithmTest \
  mdettla.jga.test.OperatorsTest \
  mdettla.jga.test.UtilsTest
fi
