
run: gridgen.F90
	gfortran gridgen.F90 -o gridgen
	./gridgen
	~/daten/Promotion/TOOLS/bin/git_ascii2bin



all:
	gridgen file

clean:
	rm -f *.vtk *.bin *.dat *.pyc gridgen
