libs=lib/bcel.jar:\
lib/GraphLayout.jar:\
lib/jadex_examples.jar:\
lib/jadex_rt.jar:\
lib/jadex_tools.jar:\
lib/janino.jar:\
lib/jhall.jar:\
lib/jibx-bind.jar:\
lib/jibx-extras.jar:\
lib/jibx-run.jar:\
lib/nuggets.jar:\
lib/xmlpull_1_1_4.jar:\
lib/xpp3.jar:\
lib/commons-codec-1.3.jar:\
lib/crimson.jar:\
lib/http.jar:\
lib/iiop.jar:\
lib/jade.jar:\
lib/jadeTools.jar:\
lib/jadex_jadeadapter.jar

if [ "$1" == "jcc" ]
then
  java -cp .:bin:$libs jadex.adapter.jade.tools.Starter
else
  if [ $# -eq 0 ]
  then
    javac -cp $libs -d bin src/pl/edu/agh/jadex/*.java
    cp src/pl/edu/agh/jadex/*.xml bin/pl/edu/agh/jadex
  else
    java -cp .:bin:$libs jadex.adapter.jade.tools.Starter -nogui \
    translation_client:pl.edu.agh.jadex.TranslationClient\(\) \
    translator:jadex.adapter.jade.JadeAgentAdapter\(pl.edu.agh.jadex.Translator default max_storage=5\)
  fi
fi
