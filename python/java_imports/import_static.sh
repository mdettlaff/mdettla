#!/bin/bash

set -e -u

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

function add_import {
  local class=$1
  local file=$2
  sed -i "s/^\(package .*\)/\1\n\nimport $class;/" $file
}

class_and_method=`echo $1 | sed "s/.*\.\(.*\..*\)/\1/"`
method=`echo $class_and_method | sed "s/.*\.\(.*\)/\1/"`
non_static_import=`echo $1 | sed "s/\(.*\)\..*/\1/"`

file=$2

sed -i "s/import $non_static_import;//" $file
sed -i "s/$class_and_method/$method/" $file
add_import "static $1" $file
$SCRIPT_DIR/organize_imports.py $file

