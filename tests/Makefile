SUBDIRS = 1D control_file  o_grid o_grid_rectangle penn_simple  pennstate  schraeg nozzle_simple chamber_simple injector_simple
#SUBDIRS := ${shell find ./ -type d -print}
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
.PHONY: subdirs $(SUBDIRS) clean $(SUBCLEAN)
 
subdirs: $(SUBDIRS)

clean:  $(SUBCLEAN)
     
$(SUBDIRS):
	$(MAKE) -C $@

$(SUBCLEAN): %.clean:
	$(MAKE) -C $* clean
