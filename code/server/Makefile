# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com
#
#
# TODO: Convert to OTP + rebar

THRIFT_PATH = ../../lib/ScrabbleCheat.thrift
THRIFT_APP=lib/thrift
RELEASE_DIR=rel

# Silly silly silly shit to generate a release, but we do what works...
SC_APP=lib/scrabblecheat
SC_REL_EBIN=$(SC_APP)/ebin
SC_REL_INCLUDE=$(SC_APP)/include
SC_REL_PRIV=$(SC_APP)/priv


test: compile 
	rebar eunit

release: binary-gaddag compile
	rm -rf $(RELEASE_DIR)
	mkdir $(RELEASE_DIR) && cd $(RELEASE_DIR) && rebar create-node nodeid="scrabblecheat" && cd -
	chmod +x $(RELEASE_DIR)/files/erl
	chmod +x $(RELEASE_DIR)/files/nodetool
	chmod +x $(RELEASE_DIR)/files/scrabblecheat
	cp -f priv/default_release.config $(RELEASE_DIR)/reltool.config
	mkdir -p $(SC_REL_EBIN) $(SC_REL_INCLUDE) $(SC_REL_PRIV)
	cp -R ebin/* $(SC_REL_EBIN)
	cp -R include/* $(SC_REL_INCLUDE)
	cp -R priv/* $(SC_REL_PRIV)
	rebar generate
	
all: test run

run: compile
	erl -pa ebin/ -pa $(THRIFT_APP)/ebin/ 

shell: compile
	erl -pa ebin/ -pa $(THRIFT_APP)/ebin/

thrift-generate: thrift-compile prepare
	[[ -d include ]] || mkdir include
	mv gen-erl/*.hrl include/
	mv gen-erl/*.beam ebin/
	cp -R $(THRIFT_APP)/include/*.hrl include/
	rm -rf gen-erl

thrift-compile: 
	thrift -r --gen erl $(THRIFT_PATH)
	erlc -o gen-erl gen-erl/*.erl

prepare:
	test -d ebin || mkdir ebin

compile: thrift-generate
	rebar compile

binary-gaddag: compile
	[ -f priv/twl06.gaddag ] || erl -noshell -pa ebin/ -s scrabblecheat_main make_binary_gaddag  -run erlang halt

clean:
	rm -f include/thrift_*.hrl include/scrabbleCheat_*.hrl 
	rm -rf $(RELEASE_DIR)
	rm -rf $(SC_APP)
	rm -f priv/twl06.gaddag priv/sowpods.gaddag priv/zynga.gaddag
	rebar clean || 0

