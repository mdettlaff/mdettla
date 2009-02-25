@echo off

javac -target 1.5 *.java
if exist *.class jar -cf TypingTest.jar *.class
if exist *.class del *.class
