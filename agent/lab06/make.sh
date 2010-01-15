jadepath="../jade/lib"
cp="$jadepath/jade.jar:\
$jadepath/http.jar:$jadepath/iiop.jar:\
$jadepath/jadeTools.jar:\
$jadepath/commons-codec-1.3.jar:\
lib/jadeMisc.jar"

if [ $# -eq 0 ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  javac -d bin -cp $cp src/mdettla/englishauction/*.java \
  src/mdettla/englishauction/ontology/*.java
else
  java -cp $cp:bin jade.Boot -detect-main false \
  seller:mdettla.englishauction.Seller\(10 30\) \
  buyer1:mdettla.englishauction.Buyer\(20 100\) \
  buyer2:mdettla.englishauction.Buyer\(30 125\) \
  buyer3:mdettla.englishauction.Buyer\(40 150\)
  mv APDescription.txt MTPs-Main-Container.txt bin
fi
