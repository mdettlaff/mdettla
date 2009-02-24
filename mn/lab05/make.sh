g95 trapez.f95 -o trapez
./trapez
#./trapez > output
#if [ "`diff output out_ok`" == "" ]
#then
#  echo OK
#else
#  echo FAIL
#fi
