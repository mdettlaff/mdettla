if [ $# -eq 0 ]
then
  javac -d bin -sourcepath src -cp ../JGA/bin -Xlint:unchecked src/*.java
else
  java -cp ../JGA/bin:bin GAMaxIndependentSet
fi
