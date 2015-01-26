SUBDIRS = 10x4  20x10  4x3-gerade  control_file  linear  o_grid  penn_simple  pennstate  schraeg
#SUBDIRS := ${shell find ./ -type d -print}
SUBCLEAN = $(addsuffix .clean,$(SUBDIRS))
.PHONY: subdirs $(SUBDIRS)

clean:  $(SUBCLEAN)
     
subdirs: $(SUBDIRS)
     
$(SUBDIRS):
	$(MAKE) -C $@

$(SUBCLEAN): %.clean:
	$(MAKE) -C $* clean
