#!/bin/bash

DIR="$( cd "$( dirname "$0" )" && pwd )"

set -u

java -cp $DIR/bin scripts.KeyLayoutGen /tmp/$1.png
convert /tmp/$1.png $DIR/../../fig/$1.jpg

