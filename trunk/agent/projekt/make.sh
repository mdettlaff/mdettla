libs=/home/mdettla/manta/agent/projekt/jadex-0.96/lib/bcel.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/GraphLayout.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jadex_examples.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jadex_rt.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jadex_tools.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/janino.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jhall.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jibx-bind.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jibx-extras.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/jibx-run.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/nuggets.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/xmlpull_1_1_4.jar:\
/home/mdettla/manta/agent/projekt/jadex-0.96/lib/xpp3.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/commons-codec-1.3.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/crimson.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/http.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/iiop.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/jade.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/jadeTools.jar:\
/home/mdettla/manta/agent/projekt/jadex-jadeadapter-0.96/lib/jadex_jadeadapter.jar

if [ "$1" == "jcc" ]
then
  java -cp .:bin:$libs jadex.adapter.jade.tools.Starter
else
  if [ $# -eq 0 ]
  then
    javac -cp $libs -d bin src/pl/edu/agh/jadex/*.java
    cp src/pl/edu/agh/jadex/*.xml bin/pl/edu/agh/jadex
  else
    java -cp .:bin:$libs jadex.adapter.jade.tools.Starter \
    translation_client:pl.edu.agh.jadex.TranslationClient\(\) \
    translator:jadex.adapter.jade.JadeAgentAdapter\(pl.edu.agh.jadex.Translator default max_storage=5\)
  fi
fi
