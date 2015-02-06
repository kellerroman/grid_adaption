FC=gfortran

HOMEDIR=../../

CFLAGS += -O3
CFLAGS += -fbounds-check -fcheck-array-temporaries -Wall
CFLAGS += -DDEBUG

GRID_ADAPT=$(HOMEDIR)bin/grid_adapt

.PHONY: all cmp run gridgen simp_git clean cp

	
all: cmp run

cmp:
	@$(MAKE) -C $(HOMEDIR)

run: gridgen
	@echo 'Running Grid-Adaper on simple Chamber'
	@$(GRID_ADAPT) #-debug

gridgen: simp_git
	./simp_git -gi git_pennstate.bin -go git.bin
	
simp_git: simp_git.F90
	@$(FC) $(CFLAGS) $< -o $@
	
clean:
	@rm -vf git_out.bin simp_git git.bin *.o *.mod *.ufo *.dat *.plt *.png *.vtk *.txt 
	@rm -vrf *~

##### CLEAN PROJECT
cp: 
	@echo "CLEANING SOLVER DIR"
	@make -C $(MAIN_DIR) clean