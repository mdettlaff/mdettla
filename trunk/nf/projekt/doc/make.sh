#FILENAME=sprawozdanie

echo "tworzenie wykresów..."
gnuplot < fuzzy_f1.gp
gnuplot < fuzzy_f1b.gp
gnuplot < fuzzy_f2.gp
gnuplot < neuro_f1.gp
gnuplot < neuro_f1b.gp
gnuplot < neuro_f2.gp
#echo "tworzenie pliku PDF..."
#pdflatex $FILENAME.tex > /dev/null
#rm $FILENAME.aux $FILENAME.log
#echo "zapisano do pliku $FILENAME.pdf"
