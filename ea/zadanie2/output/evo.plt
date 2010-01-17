set term png
set xlabel "pokolenie"
set ylabel "przystosowanie"

set yr [0:1500]

set output "griewank_csa.png"
set title "Strategia ewolucyjna - Cumulative Step Adaptation\nfunkcja Griewanka"
plot "griewank_csa" title "¶rednie przystosowanie najlepszego osobnika w populacji" with lines

set output "griewank_de.png"
set title "Algorytm genetyczny - Differential Evolution\nfunkcja Griewanka"
plot "griewank_de" title "¶rednie przystosowanie najlepszego osobnika w populacji" with lines

set yr [0:1000000000]

set output "rosenbrock_csa.png"
set title "Strategia ewolucyjna - Cumulative Step Adaptation\nfunkcja Rosenbrocka"
plot "rosenbrock_csa" title "¶rednie przystosowanie najlepszego osobnika w populacji" with lines

set output "rosenbrock_de.png"
set title "Algorytm genetyczny - Differential Evolution\nfunkcja Rosenbrocka"
plot "rosenbrock_de" title "¶rednie przystosowanie najlepszego osobnika w populacji" with lines
