FC=gfortran

MAIN_DIR=../../

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=$(MAIN_DIR)bin/grid_adapt
.PHONY:  all cmp run gridgen clean cp 

	
all: cmp gridgen run

cmp:
	make -C $(MAIN_DIR)

run:
	@echo 'Running Grid-Adaper on O-Grid'
	$(GRID_ADAPT) #-debug

gridgen:
	cd gridgen; make run
	
clean:
	rm -vf *.o git_out.bin *.mod *.ufo *.dat *.plt *.png *.vtk *.txt
	rm -vrf *~
	make -C gridgen clean

##### CLEAN PROJECT
cp: 
	@echo "CLEANING SOLVER DIR"
	@make -C $(MAIN_DIR) clean
