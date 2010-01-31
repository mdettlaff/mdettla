#./make.sh
#./make.sh run
#./make.sh all

main_class=Main

function move_swf_files_to_bin {
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
}

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
      if [ $? != 0 ]
      then
        break
      fi
      move_swf_files_to_bin
    fi
  done
else
  flashplayer bin/$main_class.swf
fi
