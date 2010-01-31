#./make.sh
#./make.sh run
#./make.sh all

main_class=HelloWorld

if [ $# == 0 ] || [ "$1" == "all" ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  source_files=`find src -name '*.as' -or -name '*.mxml'`
  for source_file in $source_files
  do
    base_path_name=${source_file#src/}
    base_path_name=${base_path_name%.as}
    base_path_name=${base_path_name%.mxml}
    binary_file=bin/$base_path_name.swf
    if [ $source_file -nt $binary_file ] || [ "$1" == "all" ]
    then
      mxmlc -sp=src --show-actionscript-warnings=true --strict=true \
        $source_file
      # begin przenosimy pliki swf do katalogu bin
      base_name=${base_path_name/*\//}
      binary_dir=${binary_file/\/$base_name.swf/}
      if [ ! -d $binary_dir ]
      then
        mkdir -p $binary_dir
      fi
      if [ -f src/${binary_file#bin/} ]
      then
        mv src/${binary_file#bin/} $binary_file
      fi
      # end przenosimy pliki swf do katalogu bin
    fi
  done
else
  flashplayer bin/$main_class.swf
fi
