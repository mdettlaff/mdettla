jadepath="../jade/lib"
cp="$jadepath/jade.jar:\
$jadepath/http.jar:$jadepath/iiop.jar:\
$jadepath/jadeTools.jar:\
$jadepath/commons-codec-1.3.jar"

if [ $# -eq 0 ]
then
  javac -d bin -cp $cp *.java
else
  java -cp $cp:bin jade.Boot -detect-main false \
  jobagent:JobAgent\(3\) tempagent1:TemperatureAgent\(20\) \
  tempagent2:TemperatureAgent\(25\) tempagent3:TemperatureAgent\(30\)
  mv APDescription.txt MTPs-Main-Container.txt bin
fi
