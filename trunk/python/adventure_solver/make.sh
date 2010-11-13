FILE=larry1

if [ $# -eq 0 ]
then
  if [ -f $FILE.png ]
  then
    rm $FILE.png
  fi
  ditaa $FILE.txt
else
  eog $FILE.png
fi

