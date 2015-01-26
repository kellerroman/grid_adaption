FC=gfortran

CFLAGS+= -DSILENT #Unnecessary comments are oppressed -DCOMMENTS
CFLAGS += -O3
CFLAGS += -DDEBUG
CFLAGS += -fbounds-check  -Wall
#-fcheck-array-temporaries
_SOURCES= global.F90 \
		  main.F90 \
		  input.F90 \
		  gradient.F90 \
		  resize.F90 \
		  calc_grid.F90 \
		  output.F90 \
		  unstr2str.F90 \
		  input_control.F90 \
		  input_grid_old.F90 \
		  calc_schwerpunkte.F90 \
		  str2unstr.F90 \
		  input_grid.F90 \
		  input_sol.F90 \
		  output_grid.F90 \
		  calc_schiebespannung.F90 \
		  input_randbed.F90
SOURCE_DIR = src

SOURCES = $(patsubst %,$(SOURCE_DIR)/%,$(_SOURCES))

_OBJECTS=$(_SOURCES:.F90=.o)
OBJECTS_DIR = obj
OBJECTS = $(patsubst %,$(OBJECTS_DIR)/%,$(_OBJECTS))

EXECUTABLE = grid_adapt
EXECUTABLE_DIR = bin

POSTPROCESSOR = multi2tec
POSTPROCESSOR_FILE = sol_plot.plt
RUN_DIR = .

.PHONY: clean all 

$(EXECUTABLE): $(OBJECTS)
	$(FC) $^ -o ./$(EXECUTABLE_DIR)/$@
	
all: $(EXECUTABLE)
	@echo 'Making Grid Adapter';
	@./$(EXECUTABLE_DIR)/$(EXECUTABLE) tests



$(OBJECTS_DIR)/%.o: $(SOURCE_DIR)/%.F90
	$(FC) -c $(CFLAGS) $(COMP_SWITCH) $^ -J$(OBJECTS_DIR) -o $@

clean:
	rm -vf $(EXECUTABLE_DIR)/$(EXECUTABLE)
	rm -vf $(EXECUTABLE_DIR)/git2para
	rm -vf $(OBJECTS_DIR)/*.o
	rm -vf $(OBJECTS_DIR)/*.mod
	rm -vf *.mod
	rm -vrf *~
	rm -vf $(SOURCE_DIR)/*~


debug: CFLAGS += -g
debug: $(EXECUTABLE)

test:
	@$(FC) tests/gridtest.F90 -o tests/gridtest
	cd tests; ./gridtest
	
git2para: src/git2paraview.F90
	$(FC) src/git2paraview.F90 -o $(EXECUTABLE_DIR)git2para