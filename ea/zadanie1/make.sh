if [ $# -eq 0 ]
then
  javac -d bin -sourcepath src -cp ../JGA/bin:lib/commons-math-2.0.jar \
  -Xlint:unchecked src/*.java
else
  java -cp ../JGA/bin:lib/commons-math-2.0.jar:bin GAMaxIndependentSet
fi
