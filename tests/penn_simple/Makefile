FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

	
all:  run_tool cmp run
#tool run_tool
    
run_tool: tool
	./simp_git -gi git_pennstate.bin -go git.bin
tool:
	$(FC) -O3 simp_git.F90 -o simp_git

cmp:
	cd ../..; make

run:
	@echo 'Running Grid-Adaper on 20x10 Grid'
	$(GRID_ADAPT) #-debug
	
clean:
	rm -vf *.o *.mod *.ufo *.dat *.plt *.png *.vtk *.txt simp_git git.bin git_out.bin
	rm -vrf *~
