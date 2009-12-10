jadepath="../jade/lib"
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
    java -cp $cp:bin jade.Boot -gui \
    tempagent1:TemperatureAgent\(20\) \ tempagent2:TemperatureAgent\(25\) \
    tempagent3:TemperatureAgent\(30\)
  else
    if [ $1 == "run2" ]
    then
      echo "Odpalam JobAgent na innym kontenerze."
      java -cp $cp:bin jade.Boot -container jag:JobAgent\(3\)
      mv APDescription.txt MTPs-Main-Container.txt bin
    fi
  fi
fi
