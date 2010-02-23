counter=5
if [ "$1" != "" ]
then
  counter=$1
fi
echo "PID = $$, counter=$counter"
sleep 2
counter=$[$counter-1]
if [ $counter -gt 0 ]
then
  ./forkbomb.sh $counter &
  ./forkbomb.sh $counter &
fi
