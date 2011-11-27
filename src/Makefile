BUILD_DIR=$(shell cd .. && echo `pwd`/build)
export BUILD_DIR

PATH:=$(BUILD_DIR)/flex/bin:$(BUILD_DIR)/bison/bin:$(BUILD_DIR)/noweb/bin:$(PATH)
export PATH

.PHONY: contrib ALL

ALL : contrib

contrib :
	cd contrib && $(MAKE)
