# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com
#
#
# TODO: Convert to OTP + rebar

THRIFT_PATH = ../../lib/ScrabbleCheat.thrift
THRIFT_APP=lib/thrift_erl
RELEASE_DIR=rel

test: compile 
	rebar eunit

release: binary-gaddag compile
	rm -rf $(RELEASE_DIR)
	mkdir $(RELEASE_DIR) && cd $(RELEASE_DIR) && rebar create-node nodeid="scrabblecheat" && cd -
	chmod +x $(RELEASE_DIR)/files/erl
	chmod +x $(RELEASE_DIR)/files/nodetool
	chmod +x $(RELEASE_DIR)/files/scrabblecheat
	cp -f priv/default_release.config $(RELEASE_DIR)/reltool.config
	rebar generate
	
all: test run

run: compile
	erl -pa ebin/ -pa $(THRIFT_APP)/ebin/ 

shell: compile
	erl -pa ebin/ -pa $(THRIFT_APP)/ebin/

thrift-generate: thrift-compile
	mv gen-erl/*.hrl include/
	mv gen-erl/*.beam ebin/
	cp -R lib/thrift-erl/include/*.hrl include/
	rm -rf gen-erl

thrift-compile: 
	thrift -r --gen erl $(THRIFT_PATH)
	erlc -o gen-erl gen-erl/*.erl

compile: thrift-generate
	rebar compile

binary-gaddag: compile
	erl -noshell -pa ebin/ -s scrabblecheat_main make_binary_gaddag  -run erlang halt

clean:
	rebar clean || 0
	rm -f include/thrift_*.hrl include/scrabbleCheat_*.hrl ebin/gaddag.dict 
	rm -rf $(RELEASE_DIR)

