@echo off

echo %1
echo asemblacja:
tasm\tasm.exe %1.asm
echo linkowanie:
tasm\tlink.exe %1.obj
echo.
if exist %1.exe echo SUKCES: program %1 zostal utworzony
if not exist %1.exe echo BLAD: program %1 nie zostal utworzony

rem x:\com\bc31\bin\tasm.exe zad.asm
rem x:\com\bc31\bin\tlink.exe zad.obj
