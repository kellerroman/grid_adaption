FC=gfortran

HOMEDIR=../../

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=$(HOMEDIR)bin/grid_adapt

.PHONY: all cmp run gridgen clean cp

	
all: cmp run

cmp:
	@$(MAKE) -C $(HOMEDIR)

run: gridgen
	@echo 'Running Grid-Adaper on simple Chamber'
	@$(GRID_ADAPT) #-debug

gridgen:
	@$(MAKE) -C gridgen run
	
clean:
	@rm -vf git_out.bin *.o *.mod *.ufo *.dat *.plt *.png *.vtk *.txt 
	@rm -vrf *~
	@$(MAKE) -C gridgen clean

##### CLEAN PROJECT
cp: 
	@echo "CLEANING SOLVER DIR"
	@make -C $(MAIN_DIR) clean