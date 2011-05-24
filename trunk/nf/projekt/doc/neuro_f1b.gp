set terminal png size 640, 480 crop
set output "neuro_f1b.png"
#set label ""
#set key left
set grid
plot "neuro_f1b_expected.dat" using 1:2

set title "f1, eps = 0.1, propagacja wsteczna"
set output "neuro_f1b.png"
replot "neuro_f1b_result.dat" using 1:2

