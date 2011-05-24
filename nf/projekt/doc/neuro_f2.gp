set terminal png size 640, 480 crop
set output "neuro_f2.png"
#set label ""
#set key left
set grid
plot "neuro_f2_expected.dat" using 1:2

set title "f2, propagacja wsteczna"
set output "neuro_f2.png"
replot "neuro_f2_result.dat" using 1:2

