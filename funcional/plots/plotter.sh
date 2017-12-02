set terminal png size 1200,900;
set output 'out.png'


set title "N-body comparisons"

set yrange[0:100]
set xrange[0:10]

plot "out.dat" using 1:2 title "Samples", \
     x*(x-1) title "Brute Force"

#pause -1 "Hit any key to continue"

