FC=gfortran

run: gridgen
	./gridgen

gridgen: gridgen.F90
	$(FC) $< -o $@

.PHONY: run gridgen clean

clean:
	@rm -f git.* gridgen *~  *.vtk *.lay *.plt *.mod

paraview: run
	../../../bin/sol2para -git
