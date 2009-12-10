jadepath="../../jade/lib"
cp="$jadepath/jade.jar:\
$jadepath/http.jar:$jadepath/iiop.jar:\
$jadepath/jadeTools.jar:\
$jadepath/commons-codec-1.3.jar"

if [ $# -eq 0 ]
then
  javac -d bin -cp $cp *.java
else
  java -cp $cp:bin jade.Boot -detect-main false \
  cthulhu:Cthulhu\(\) cultist:Cultist\(\)
  mv *.txt bin
fi
