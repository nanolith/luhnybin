.PHONY ALL

ALL : .bison-install

.bison-install : .bison-build
	(cd bison && $(MAKE) install)
	touch .bison-install

.bison-build : .bison-config
	(cd bison && $(MAKE))
	touch .bison-build

.bison-config :
	(cd bison && ./configure --PREFIX=$(BUILD_DIR)/bison)
	touch .bison-config
