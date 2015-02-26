SUBDIRS = control_file 1D schraeg o_grid o_grid_rectangle o_grid_rectangle_with_normal nozzle_simple chamber_simple_wo_nozzle chamber_simple injector_simple methan_v1 penn_simple penn_wo_nozzle pennstate 3D/o_grid
#SUBDIRS := ${shell find ./ -type d -print}
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
.PHONY: subdirs $(SUBDIRS) clean $(SUBCLEAN)
 
subdirs: $(SUBDIRS)

clean:  $(SUBCLEAN)
     
$(SUBDIRS):
	$(MAKE) -C $@

$(SUBCLEAN): %.clean:
	$(MAKE) -C $* clean
