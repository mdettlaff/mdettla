if [ -f dane ]
then
  rm dane
fi
if [ -f wyniki ]
then
  rm wyniki
fi
gcc -Wall client.c -o client
gcc -Wall server.c -o server
