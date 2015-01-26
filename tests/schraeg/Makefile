FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

	
all: gridgen cmp run

cmp:
	make -C ../../

run:
	@echo 'Running Grid-Adaper on schraeges Gitter'
	$(GRID_ADAPT) #-debug

gridgen:
	make -C gridgen run
	
clean:
	rm -vf *.o *.mod *.ufo  *.dat *~ *.plt *.png *.vtk *.txt git_out.bin
	make -C gridgen clean
