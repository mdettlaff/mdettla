if [ $# -eq 0 ]
then
  javac -d bin -sourcepath src -cp ../JGA/bin:lib/commons-math-2.0.jar \
  -Xlint:unchecked src/*.java
else
  if [ $1 -eq 1 ]
  then
    java -cp bin:../JGA/bin:lib/commons-math-2.0.jar GAMaxIndependentSet
  else
    java -cp bin:../JGA/bin MaxIndependentSet
  fi
fi
