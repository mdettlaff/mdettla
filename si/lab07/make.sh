if [ ! -d Sat/bin ]
then
  mkdir Sat/bin
fi
cd Sat
javac -d bin src/*.java
cd ..
