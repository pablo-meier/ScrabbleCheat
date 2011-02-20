# Dare I delve into... recursive Make?  Ruh-roh...
#
# Makefile for the AS3 AIR client for ScrabbleCheat.  Uses
# Thrift to connect to the Erlang server!

SRC=as
BUILD=build

CONFIG=config.xml

APP_XML=scrabblecheat-app.xml

compile: prepare
	mxmlc --load-config=$(CONFIG) $(SRC)/com/morepaul/ScrabbleCheat/*.as

run:
	adl $(APP_XML)

clean:
	rm -rf $(BUILD)

prepare: 
	test -d $(BUILD) || mkdir $(BUILD)
