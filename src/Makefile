BUILD_DIR=$(shell cd .. && echo `pwd`/build)
export BUILD_DIR

.PHONY contrib ALL

ALL : contrib

contrib :
	cd contrib && $(MAKE)
