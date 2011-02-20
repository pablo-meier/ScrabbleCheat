# Top-level Makefile for all things ScrabbleCheat.
# 
# February 2011

BUILD_DIR=build
SERVER=code/server

CLIENTS=code/clients
FLASH_CLIENT=$(CLIENTS)/flash
CURSES_CLIENT=$(CLIENTS)/curses



test:
	echo "run all the tests"

make-curses-client:
	echo "make the cureses client"

make-flash-client:
	echo "make the flash client"

make-server:
	echo "make the server"

clean:
	rm -rf $(BUILD_DIR)

prepare: 
	[[ -d $(BUILD_DIR) ]] || mkdir $(BUILD_DIR)

