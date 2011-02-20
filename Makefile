# Top-level Makefile for all things ScrabbleCheat.
# 
# February 2011

BUILD_DIR=build
SERVER=code/server

CLIENTS=code/clients
FLASH_CLIENT=$(CLIENTS)/flash
CURSES_CLIENT=$(CLIENTS)/curses





clean:
	rm -rf $(BUILD_DIR)

prepare: 
	[[ -d $(BUILD_DIR) ]] || mkdir $(BUILD_DIR)

