OBJECTS_DIR= obj
REALMAKEFILE=../src/Makefile.real
solver: FORCE
	@(cd $(OBJECTS_DIR) && $(MAKE) -f $(REALMAKEFILE))

clean:
	@(cd $(OBJECTS_DIR) && $(MAKE) -f $(REALMAKEFILE) clean)
	@rm -rf *~
	
FORCE:
	@mkdir -p obj bin