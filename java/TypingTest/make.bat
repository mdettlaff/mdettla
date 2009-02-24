@echo off

javac -target 1.5 *.java
if not exist txt\* echo Brak katalogu "txt" z tekstami
if exist *.class if exist txt\* jar -cf TypingTest.jar *.class txt
if exist *.class del *.class
