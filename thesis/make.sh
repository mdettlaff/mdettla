SIGMA_DIR=seminarium/thesis

scp *.tex *.bib sigma:$SIGMA_DIR
if [ "$1" == "all" ]
then
  scp fig/*.* sigma:$SIGMA_DIR/fig
fi
ssh sigma "cd $SIGMA_DIR && make"
scp sigma:$SIGMA_DIR/magisterka.pdf /tmp
