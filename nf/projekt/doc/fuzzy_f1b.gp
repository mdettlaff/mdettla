set terminal png size 640, 480 crop
set output "fuzzy_f1b.png"
#set label ""
#set key left
set grid
plot "fuzzy_f1b_expected.dat" using 1:2

set title "f1, eps = 0.1, proste reguly rozmyte"
set output "fuzzy_f1b.png"
replot "fuzzy_f1b_result.dat" using 1:2

