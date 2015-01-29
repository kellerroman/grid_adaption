FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

EXECUTABLE = 20x10
GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

all: cmp_prog test

cmp_prog:
	cd ../..; make

test:
	@echo 'Running Grid-Adaper on 20x10 Grid'
	$(GRID_ADAPT) #-debug
	
clean:
	rm -vf $(EXECUTABLE)
	rm -vf *.o
	rm -vf *.mod
	rm -vf *.ufo 
	rm -vf *.dat
	rm -vrf *~
	rm -vf *.plt
	rm -vf *.png
	rm -vf *.vtk
