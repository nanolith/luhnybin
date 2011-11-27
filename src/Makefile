BUILD_DIR=$(shell cd .. && echo `pwd`/build)
export BUILD_DIR

PATH:=$(BUILD_DIR)/flex/bin:$(BUILD_DIR)/bison/bin:$(BUILD_DIR)/noweb/bin:$(PATH)
export PATH

.PHONY: contrib cxxfilter ALL

ALL : contrib cxxfilter

contrib :
	cd contrib && $(MAKE)

cxxfilter :
	cd cxxfilter && $(MAKE)
