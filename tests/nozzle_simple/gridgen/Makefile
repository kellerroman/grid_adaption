run: gridgen
	./gridgen

gridgen: gridgen.F90
	gfortran gridgen.F90 -o gridgen

.PHONY: run gridgen clean
clean:
	@rm -f git.bin gridgen *~  *.vtk

paraview: run
	../../../bin/sol2para -git
