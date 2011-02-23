# Dare I delve into... recursive Make?  Ruh-roh...
#
# Makefile for the AS3 AIR client for ScrabbleCheat.  Uses
# Thrift to connect to the Erlang server!

SRC=src
BUILD=build

EXTERNAL_LIB=../../../lib

PACKAGE_PATH=com/morepaul/ScrabbleCheat
SRC_AS=$(SRC)/$(PACKAGE_PATH)

BUILD_DUMP=$(BUILD)/gen-as3/$(PACKAGE_PATH)

CONFIG=config.xml

APP_XML=scrabblecheat-app.xml

compile: compile-thrift
	cp -R $(SRC_AS)/*.as $(BUILD_DUMP)
	mxmlc --load-config=$(CONFIG) $(BUILD_DUMP)/ScrabbleCheatClientMain.as

compile-thrift: prepare
	thrift -r --gen as3 -o $(BUILD) $(EXTERNAL_LIB)/ScrabbleCheat.thrift

run:
	adl $(APP_XML)

clean:
	[[ -d $(BUILD) ]] && rm -rf $(BUILD)

prepare: 
	[[ -d $(BUILD) ]] || mkdir -p $(BUILD_DUMP)
