FC=gfortran

HOMEDIR=../../

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=$(HOMEDIR)bin/grid_adapt
.PHONY: clean all 

	
all: cmp run

cmp:
	make -C $(HOMEDIR)

run:
	@echo 'Running Grid-Adaper on O-Grid'
	$(GRID_ADAPT) #-debug

gridgen:
	cd gridgen; make run
	
cp:
	make -C $(HOMEDIR) clean
clean:
	rm -vf *.o git_out.bin *.mod *.ufo *.dat *.plt *.png *.vtk *.txt
	rm -vrf *~
	make -C gridgen clean
