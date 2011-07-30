if [ ! -d bin ]
then
    mkdir bin
fi
javac -d bin src/scripts/*.java
cp src/scripts/*.png bin/scripts
