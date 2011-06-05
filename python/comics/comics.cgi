#!/bin/bash

HOME=/home/studinf/mdettla
COMICS_DIR=$HOME/public_html/comics
LOG=$COMICS_DIR/log

echo "Content-type: text/html"
echo ""
echo "<html>"
echo "    <body>"

date >> $LOG
echo "downloading comics on demand" >> $LOG
$HOME/python/comics/comics.py $COMICS_DIR >> $LOG &

echo "        downloading comics started"
echo "    </body>"
echo "</html>"
exit 0
