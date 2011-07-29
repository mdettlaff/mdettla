SIGMA_DIR=seminarium/thesis

if [ "$1" == "" ]
then
  scp magisterka.tex xml.bib sigma:$SIGMA_DIR
  scp fig/*.* sigma:$SIGMA_DIR/fig
  ssh sigma "cd $SIGMA_DIR && make"
  scp sigma:$SIGMA_DIR/magisterka.pdf /tmp
else
  echo unknown arguments
fi
