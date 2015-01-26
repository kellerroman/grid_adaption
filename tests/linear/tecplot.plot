 set datafile separator ','
 set grid
 set term png
set output 'lines.png'
set title '1D PLOT'

plot sin(pi*x) with lines title 'FUNKTION','git_cell.dat'using 1:2 with points title 'CELLCENTERED' \
   ,'1Dout_0.dat' using 1:2 with points title 'I = 0' \
   ,'1Dout_10.dat' using 1:2 with points title 'I = 1' \
   ,'1Dout_20.dat' using 1:2 with points title 'I = 2'\
   ,'1Dout_30.dat' using 1:2 with points title 'I = 3'\
   ,'1Dout_40.dat' using 1:2 with points title 'I = 4'\
   ,'1Dout_50.dat' using 1:2 with points title 'I = 5' 
