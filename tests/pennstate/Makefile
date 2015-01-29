FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

	
all: cmp run

tool:
	$(FC) -O3 simp_git.F90 -o simp_git

cmp:
	cd ../..; make

run:
	@echo 'Running Grid-Adaper on 20x10 Grid'
	$(GRID_ADAPT) #-debug
	
clean:
	rm -vf *.o
	rm -vf *.mod
	rm -vf *.ufo 
	rm -vf *.dat
	rm -vrf *~
	rm -vf *.plt
	rm -vf *.png
	rm -vf *.vtk
	rm -vf *.txt git_out.bin
	
