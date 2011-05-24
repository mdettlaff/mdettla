set terminal png size 640, 480 crop
set output "fuzzy_f2.png"
#set label ""
#set key left
set grid
plot "fuzzy_f2_expected.dat" using 1:2

set title "f2, proste reguly rozmyte"
set output "fuzzy_f2.png"
replot "fuzzy_f2_result.dat" using 1:2

