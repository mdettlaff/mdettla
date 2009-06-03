if [ $# -eq 0 ]
then
  javac CenyDOM.java
else
  java -cp .:xercesImpl.jar CenyDOM ceny.xml standardy.xml
fi
