# Makefile for the Ruby client.  Should probably use Rakefile :-/

BUILD_DIR=build
GLOBAL_LIB=../../../lib


compile: prepare
	thrift -r -o $(BUILD_DIR) --gen rb $(GLOBAL_LIB)/ScrabbleCheat.thrift

run: compile
	ruby client.rb

prepare:
	[[ -d $(BUILD_DIR) ]] || mkdir $(BUILD_DIR)

clean:
	[[ -d $(BUILD_DIR) ]] && rm -rf $(BUILD_DIR)
