FC=gfortran

HOMEDIR=../../

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=$(HOMEDIR)bin/grid_adapt
.PHONY: clean all 

	
all: cmp run

cmp:
	$(MAKE) -C $(HOMEDIR)

run:
	@echo 'Running Grid-Adaper on simple Nozzle'
	$(GRID_ADAPT) #-debug

gridgen:
	$(MAKE) -C gridgen run
	
cp:
	$(MAKE) -C $(HOMEDIR) clean
clean:
	rm -vf *.o git_out.bin *.mod *.ufo *.dat *.plt *.png *.vtk *.txt
	rm -vrf *~
	$(MAKE) -C gridgen clean
