jadepath="../../jade/lib"
cp="$jadepath/jade.jar:\
$jadepath/http.jar:$jadepath/iiop.jar:\
$jadepath/jadeTools.jar:\
$jadepath/commons-codec-1.3.jar"

if [ $# -eq 0 ]
then
  javac -d bin -cp $cp *.java
else
  if [ $1 == "run1" ]
  then
    echo "Odpalam agenta na glownym kontenerze."
    java -cp $cp:bin jade.Boot -gui ta:MyAgent\(\)
    mv *.txt bin
  else
    if [ $1 == "run2" ]
    then
      echo "Odpalam agenta na innym kontenerze."
      java -cp $cp:bin jade.Boot -container da:DistantAgent\(\)
      mv *.txt bin
    fi
  fi
fi
