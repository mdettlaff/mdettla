set terminal png size 640, 480 crop
set output "fuzzy_f3.png"
#set label ""
#set key left
#set grid
set dgrid3d
splot "fuzzy_f3_expected.dat" using 1:2:3 with lines

set title "f3, proste reguly rozmyte"
set output "fuzzy_f3.png"
replot "fuzzy_f3_result.dat" using 1:2:3 with lines

