FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

MAIN_DIR=../../

GRID_ADAPT=$(MAIN_DIR)bin/grid_adapt

.PHONY: clean all cmp run gridgen cp
	
all: gridgen cmp run

cmp:
	@$(MAKE) -C $(MAIN_DIR)

run:
	@echo 'Running Grid-Adaper on schraeges Gitter'
	@$(GRID_ADAPT) #-debug

gridgen:
	@make -C gridgen
	
clean:
	@rm -vf *.o *.mod *.ufo  *.dat *~ *.plt *.png *.vtk *.txt git_out.bin
	@$(MAKE) -C gridgen clean

##### CLEAN PROJECT
cp: 
	@echo "CLEANING SOLVER DIR"
	@m$(MAKE) -C $(MAIN_DIR) clean
