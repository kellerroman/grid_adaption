OBJECTS_DIR= obj
REALMAKEFILE=../src/Makefile.in
TOOLMAKEFILE=../tools/Makefile.in

all: tools solver

solver: FORCE
	@(cd $(OBJECTS_DIR) && $(MAKE) -f $(REALMAKEFILE))

clean: FORCE
	@(cd $(OBJECTS_DIR) && $(MAKE) -f $(REALMAKEFILE) clean)
	@rm -rf obj bin *~
	@$(MAKE) -C tests clean
	
tools: FORCE
	@(cd $(OBJECTS_DIR) && $(MAKE) -f $(TOOLMAKEFILE))	
	
FORCE:
	@mkdir -p obj bin

test:
	@$(MAKE) -C tests 