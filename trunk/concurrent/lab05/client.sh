#!/bin/bash

if [ $# -lt 2 ]
then
  echo "Użycie: ./client.sh LICZBA NAZWA_KONTA_SERWERA"
else
  number=$1
  server_home=$2
  if [ ! -p /home/studinf/$server_home/tmp/serwerfifo ]
  then
    echo "Brak połączenia z serwerem"
    exit 1
  fi
  if [ ! -p ~/tmp/klientfifo ]
  then
    mkfifo ~/tmp/klientfifo
  fi
  echo $HOME:$number > /home/studinf/$server_home/tmp/serwerfifo
  result=`cat < ~/tmp/klientfifo`
  echo "wynik: $result"
fi
