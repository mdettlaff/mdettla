#!/bin/bash

number=$1
client_home=$2
let result=number*number
echo $result > $client_home/tmp/klientfifo
