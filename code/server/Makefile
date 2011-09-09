# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com
#
#
# TODO: Convert to OTP + rebar

THRIFT_PATH = ../../lib/ScrabbleCheat.thrift
THRIFT_APP=lib/thrift
RELEASE_DIR=rel


EUNIT_PATH=tests/eunit

# Silly silly silly shit to generate a release, but we do what works...
SC_APP=lib/scrabblecheat
SC_REL_EBIN=$(SC_APP)/ebin
SC_REL_INCLUDE=$(SC_APP)/include
SC_REL_PRIV=$(SC_APP)/priv

PARSER_PATH=lib/bingad
PARSER=$(PARSER_PATH)/bin/gaddag_parser
DICTS=priv/dicts


test: eunit ct

eunit: compile build-parser
	$(PARSER) $(EUNIT_PATH)/gaddag_test.txt
	mv $(EUNIT_PATH)/gaddag_test.dict ebin/
	$(PARSER) $(EUNIT_PATH)/testdict.txt
	mv $(EUNIT_PATH)/testdict.dict ebin/
	rebar eunit


ct: compile binary-gaddag
	[ -d rel ] || $(MAKE) release
	mkdir -p .common_test/logs
	mkdir -p .common_test/coverage
	ruby tests/common_test/server_util.rb started
	cat test.spec.in | sed -e "s,@PATH@,$(PWD)," > test.spec
	ct_run -pa ebin/ -pa lib/*/ebin -spec test.spec
	# rel/scrabblecheat/bin/scrabblecheat stop


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

binary-gaddag: build-parser
	[ -f priv/twl06.dict ] || $(PARSER) $(DICTS)/twl06.txt $(DICTS)/zynga.txt $(DICTS)/sowpods.txt
	[ -f priv/twl06.dict ] || mv $(DICTS)/*.dict priv/

build-parser:
	cd $(PARSER_PATH) && $(MAKE) compile

clean:
	rm -f include/thrift_*.hrl include/scrabbleCheat_*.hrl 
	rm -f ebin/*.dict
	ruby tests/common_test/server_util.rb stopped
	cd $(PARSER_PATH) && $(MAKE) clean
	rm -rf $(RELEASE_DIR)
	rm -rf $(SC_APP)
	rm -rf .common_test test.spec tests/common_test/*.beam
	# rm -f priv/*.dict
	rebar clean || echo "clean"

