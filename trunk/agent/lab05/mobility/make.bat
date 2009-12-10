@echo off

if "%1"=="run1" goto RUN1
if "%1"=="run2" goto RUN2
if "%1"=="clean" goto CLEAN
if "%1"=="" goto MAKE

:MAKE
javac -d bin -cp ../../jade/lib/jade.jar;../../jade/lib/http.jar;../../jade/lib/iiop.jar;../../jade/lib/jadeTools.jar;../../jade/lib/commons-codec-1.3.jar *.java
goto END

:RUN1
java -cp ../../jade/lib/jade.jar;../../jade/lib/http.jar;../../jade/lib/iiop.jar;../../jade/lib/jadeTools.jar;../../jade/lib/commons-codec-1.3.jar;bin jade.Boot -gui ta:MyAgent()
goto CLEAN
goto END

:RUN2
java -cp ../../jade/lib/jade.jar;../../jade/lib/http.jar;../../jade/lib/iiop.jar;../../jade/lib/jadeTools.jar;../../jade/lib/commons-codec-1.3.jar;bin jade.Boot -container da:DistantAgent()
goto CLEAN
goto END

:CLEAN
move *.txt bin
goto END

:END
