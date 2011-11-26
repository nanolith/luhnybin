BUILD_DIR=$(shell cd .. && echo `pwd`/build)
export BUILD_DIR

PATH:=$(BUILD_DIR)/flex/bin:$(BUILD_DIR)/bison/bin:$(BUILD_DIR)/googlecode/bin:$(PATH)
export PATH

LEX=$(BUILD_DIR)/flex/bin/flex
export LEX

.PHONY: contrib ALL

ALL : contrib

contrib :
	cd contrib && $(MAKE)
