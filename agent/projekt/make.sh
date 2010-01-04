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
    javac -cp $libs -d bin src/mdettla/jadex/pennyauctions/buyer/*.java \
    src/mdettla/jadex/pennyauctions/seller/*.java
    cp src/mdettla/jadex/pennyauctions/buyer/*.xml bin/mdettla/jadex/pennyauctions/buyer
  else
    java -cp .:bin:$libs jadex.adapter.jade.tools.Starter -nogui \
    auction_site:mdettla.jadex.pennyauctions.seller.AuctionSite\(\) \
    buyer1:jadex.adapter.jade.JadeAgentAdapter\(mdettla.jadex.pennyauctions.buyer.Buyer default\)
  fi
fi
