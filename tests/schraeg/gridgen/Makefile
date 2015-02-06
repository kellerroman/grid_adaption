FC=gfortran

#CFLAGS  = -Wconversion -Wall -Wextra -fbounds-check
CFLAGS += -O3

.PHONY: run gridgen clean paraview tecplot

run: gridgen
	@./gridgen
	
gridgen: gridgen.F90
	@$(FC) $(CFLAGS) $< -o $@

clean:
	@rm -f gridgen git.* *.vtk *.bin *.dat *.pyc *.plt *.mod *.lay *~

paraview: run
	@../../../bin/sol2para -git
	
tecplot: run
	@u2p -git