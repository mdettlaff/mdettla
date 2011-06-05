#!/bin/bash

HOME=/home/studinf/mdettla
COMICS_DIR=$HOME/public_html/comics
LOG=$COMICS_DIR/log

echo "Content-type: text/html"
echo ""
echo "<html>"
echo "    <body>"

date >> $LOG
echo "pobieranie komiksów na żądanie" >> $LOG
$HOME/python/scripts/comics.py $COMICS_DIR &

echo "        rozpoczęto pobieranie komiksów<br>"
echo "    </body>"
echo "</html>"
exit 0
