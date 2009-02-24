g95 zad4.f95 -o zad4
./zad4 > output
if [ "`diff output out_ok`" == "" ]
then
  echo OK
else
  echo FAIL
fi
