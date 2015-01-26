FC=gfortran

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

EXECUTABLE =linear 
GRID_ADAPT=../../bin/grid_adapt
.PHONY: clean all 

$(EXECUTABLE): linear.F90
	@$(FC) $^ -o ./$(EXECUTABLE_DIR)/$@


	
all: $(EXECUTABLE) run cmp_prog test

cmp_prog:
	cd ../..; make

run:
	@echo 'running lineare Testall Gridgenerator'
	@./$(EXECUTABLE)
 
test:
	@echo 'Running Grid-Adaper on linear Grid'
	$(GRID_ADAPT) # -debug 
	
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
