set terminal png size 400,300;
set output 'out.png'


set title "N-body comparisons"

set yrange[0:200]
set xrange[0:20]

plot "out.dat" using 1:2 title "Samples", \
     x*(x-1) title "Brute Force"

#pause -1 "Hit any key to continue"

