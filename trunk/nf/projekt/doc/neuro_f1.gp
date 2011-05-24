set terminal png size 640, 480 crop
set output "neuro_f1.png"
#set label ""
#set key left
set grid
plot "neuro_f1_expected.dat" using 1:2

set title "f1, eps = 0.05, propagacja wsteczna"
set output "neuro_f1.png"
replot "neuro_f1_result.dat" using 1:2

