SUBDIRS = control_file  1D schraeg o_grid o_grid_rectangle  nozzle_simple chamber_simple injector_simple penn_simple pennstate
#SUBDIRS := ${shell find ./ -type d -print}
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
.PHONY: subdirs $(SUBDIRS) clean $(SUBCLEAN)
 
subdirs: $(SUBDIRS)

clean:  $(SUBCLEAN)
     
$(SUBDIRS):
	$(MAKE) -C $@

$(SUBCLEAN): %.clean:
	$(MAKE) -C $* clean
