#./make.sh
#./make.sh run
#./make.sh all

main_class=TypingTestApp

if [ $# == 0 ] || [ "$1" == "all" ]
then
  source_files=`find src -name '*.as' -or -name '*.mxml'`
  binary_file="bin/$main_class.swf"
  changed=false
  for source_file in $source_files
  do
    if [ $source_file -nt $binary_file ]
    then
      changed=true
    fi
  done
  if $changed || [ "$1" == "all" ]
  then
    main_file=`ls src/$main_class.*`
    mxmlc -sp=src --show-actionscript-warnings=true --strict=true \
      --debug=true $main_file
    if [ $? == 0 ]
    then
      if [ ! -d bin ]
      then
        mkdir bin
      fi
      mv src/$main_class.swf bin/$main_class.swf
    fi
  fi
else
  if [ "$1" == "clean" ]
  then
    rm -r bin
  else
    flashplayer bin/$main_class.swf
  fi
fi
