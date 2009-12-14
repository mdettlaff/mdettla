if [ $# -eq 0 ]
then
  if [ ! -d bin ]
  then
    mkdir bin
  fi
  javac -d bin src/mdettla/imgproc/*.java \
  src/mdettla/imgproc/fuzzy/edge/*.java \
  src/mdettla/imgproc/edge/*.java
else
  if [ $1 == "edge" ]
  then
    java -cp bin mdettla.imgproc.ProcessImage \
    mdettla.imgproc.edge.SobelAlgorithm $2 $3
  else
    if [ $1 == "fuzzyedge" ]
    then
      java -cp bin mdettla.imgproc.ProcessImage \
      mdettla.imgproc.fuzzy.edge.EdgeDetectionWithFuzzyClassifier $2 $3
    else
      echo "Nieznany parametr"
    fi
  fi
fi

