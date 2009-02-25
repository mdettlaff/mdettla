#!/bin/bash

javac -target 1.5 *.java
if [ -e TypingTest.class ]
then
	jar -cf TypingTest.jar *.class
fi
if [ -e TypingTest.class ]
then
	rm *.class
fi
