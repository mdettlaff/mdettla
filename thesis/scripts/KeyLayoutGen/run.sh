#!/bin/bash

set -u

java -cp bin scripts.KeyLayoutGen /tmp/$1.png
convert /tmp/$1.png ../../fig/$1.jpg

