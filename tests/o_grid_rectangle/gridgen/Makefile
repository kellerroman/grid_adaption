FC=gfortran

#CFLAGS  = -Wconversion -Wall -Wextra -fbounds-check
CFLAGS += -O3
.PHONY: run gridgen clean


run: gridgen
	./gridgen
	
gridgen: gridgen.F90
	@$(FC) $(CFLAGS) $< -o $@
#	~/daten/Promotion/TOOLS/bin/git_ascii2bin

clean:
	rm -f *.vtk *.bin *.dat *.pyc gridgen
