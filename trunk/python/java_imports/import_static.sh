#!/bin/bash

set -e -u

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"

function add_import {
  local class=$1
  local file=$2
  sed -i "s/^\(package .*\)/\1\n\nimport $class;/" $file
}

if [ $# -lt 2 ]
then
  echo "Usage: $0 STATIC_METHOD JAVA_FILES..."
  exit 2
fi

class_and_method=`echo $1 | sed "s/.*\.\(.*\..*\)/\1/"`
method=`echo $class_and_method | sed "s/.*\.\(.*\)/\1/"`
non_static_import=`echo $1 | sed "s/\(.*\)\..*/\1/"`

files=( $@ )
unset files[0]

for file in ${files[@]}
do
  if grep -q "$class_and_method(" $file
  then
    sed -i "s/import $non_static_import;//" $file
    sed -i "s/$class_and_method(/$method(/" $file
    add_import "static $1" $file
    $SCRIPT_DIR/organize_imports.py $file
  fi
done

