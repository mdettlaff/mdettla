set terminal png size 640, 480 crop
set output "fuzzy_f1.png"
#set label ""
#set key left
set grid
plot "fuzzy_f1_expected.dat" using 1:2

set title "f1, eps = 0.05, proste reguly rozmyte"
set output "fuzzy_f1.png"
replot "fuzzy_f1_result.dat" using 1:2

