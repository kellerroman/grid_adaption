FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

EXECUTABLE = 4x3-gerade
GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

$(EXECUTABLE): 4x3-gerade.F90
	@$(FC) $^ -o ./$(EXECUTABLE_DIR)/$@
	
all: $(EXECUTABLE) cmp_prog run test

cmp_prog:
	cd ../..; make
run:
	@echo 'running 4x3-gerade Gridgenerator'
	@./$(EXECUTABLE)
 
test:
	@echo 'Running Grid-Adaper on 4x3-gerade Grid'
	$(GRID_ADAPT) -debug
	
clean:
	rm -vf $(EXECUTABLE)
	rm -vf *.o
	rm -vf *.mod
	rm -vf *.ufo 
	rm -vf *.dat
	rm -vrf *~
	rm -vf *.plt
	rm -vf *.png
	rm -vf *.vtk
