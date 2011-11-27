.PHONY: ALL

ALL : .flex-install .bison-install .noweb-install

.noweb-install : .noweb-build
	(cd noweb && $(MAKE) install)
	touch .noweb-install

.noweb-build :
	(cd noweb && $(MAKE))
	touch .noweb-build

.flex-install : .flex-build
	(cd flex && $(MAKE) install)
	touch .flex-install

.flex-build : .flex-config
	(cd flex && $(MAKE))
	touch .flex-build

.flex-config :
	touch flex/Makefile.in
	touch flex/configure
	(cd flex && ./configure --prefix=$(BUILD_DIR)/flex)
	touch .flex-config

.bison-install : .flex-install .bison-build
	(cd bison && $(MAKE) install)
	touch .bison-install

.bison-build : .bison-config
	(cd bison && $(MAKE))
	touch .bison-build

.bison-config :
	touch bison/Makefile.in
	touch bison/configure
	(cd bison && export LEX=$(BUILD_DIR)/flex/bin/flex && ./configure --prefix=$(BUILD_DIR)/bison)
	touch .bison-config
