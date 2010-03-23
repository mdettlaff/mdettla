#!/bin/bash

trap "exit 0" SIGUSR1
trap "" SIGHUP
trap "" SIGTERM

if [ ! -p ~/tmp/serwerfifo ]
then
  mkfifo ~/tmp/serwerfifo
fi
while true
do
  client_msg=`cat < ~/tmp/serwerfifo`
  IFS=":"
  tokens=($client_msg)
  client_home=${tokens[0]}
  number=${tokens[1]}
  ./server_worker.sh $number $client_home &
done
