# Top-level Makefile for all things ScrabbleCheat.
# 
# February 2011

BUILD_DIR=build
SERVER=code/server

CLIENTS=code/clients
FLASH_CLIENT=$(CLIENTS)/flash
CURSES_CLIENT=$(CLIENTS)/curses




test-and-build: build test

test: test-server
	
build: make-server make-curses-client make-flash-client


make-server: prepare build-server
	cp -R $(SERVER)/ebin $(BUILD_DIR)

test-server: build-server
	cd $(SERVER); $(MAKE) test; cd -

build-server: build-server
	cd $(SERVER); $(MAKE) compile; cd -



make-curses-client:
	cd $(CURSES_CLIENT); $(MAKE); cd -




make-flash-client:
	cd $(FLASH_CLIENT); $(MAKE); cd -




clean:
	cd $(SERVER); $(MAKE) clean; cd -
	cd $(CURSES_CLIENT); $(MAKE) clean; cd -
	cd $(FLASH_CLIENT); $(MAKE) clean; cd -
	rm -rf $(BUILD_DIR)

prepare: 
	[[ -d $(BUILD_DIR) ]] || mkdir $(BUILD_DIR)

