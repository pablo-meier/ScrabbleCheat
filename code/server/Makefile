# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com
#
#
# TODO: Convert to OTP + rebar

BUILD_BEAM_DIR = $(BUILD)/bin
BUILD_TEST_DIR = $(BUILD)/tests

TESTDIR = test

SRCDIR = src
LIBDIR = lib
ERLC = erlc

THRIFT_PATH = ../../lib/ScrabbleCheat.thrift

ERL_THRIFT_INCLUDE = $(LIBDIR)/thrift-erl
ERL_THRIFT_GEN = $(BUILD)/gen-erl
ERL_THRIFT_BEAM_OUTPUT = $(BUILD)/thrift-beam
ERL_THRIFT_INCLUDES = -I $(ERL_THRIFT_INCLUDE) -I $(ERL_THRIFT_GEN)
ERL_THRIFT_COMPILE_FLAGS = $(ERL_THRIFT_INCLUDES) -o $(ERL_THRIFT_BEAM_OUTPUT) 

ERLC_SRC_FLAGS = -v -o $(BUILD_BEAM_DIR) $(ERL_THRIFT_INCLUDES) 
ERLC_TEST_FLAGS = -v -o $(BUILD_TEST_DIR) $(ERL_THRIFT_INCLUDES)

ERL_TEST_FLAGS = -noshell -pa $(BUILD_BEAM_DIR) -pa $(ERL_THRIFT_BEAM_OUTPUT) -pa $(BUILD_TEST_DIR)

ERL_FLAGS = $(ERL_INCLUDE_FLAGS) $(ERL_RUN_FLAGS)
ERL_START = erl +K true
ERL_END = -run erlang halt
ERL_RUN = $(ERL) $(ERL_FLAGS)



run: compile
	erl -pa ebin/ -pa priv/thrift-erl/ebin/

binary-gaddag: compile
	$(ERL_START) -noshell -pa $(BUILD_BEAM_DIR) -run main make_binary_gaddag $(ERL_END)

thrift-generate: thrift-compile
	mv gen-erl/*.hrl include/
	mv gen-erl/*.beam ebin/
	cp -R priv/thrift-erl/include/*.hrl include/
	# cp -R priv/thrift-erl/ebin/*.beam ebin/
	rm -rf gen-erl

thrift-compile: 
	thrift -r --gen erl ~/Desktop/projects/ScrabbleCheat/lib/ScrabbleCheat.thrift
	erlc -o gen-erl gen-erl/*.erl

compile: thrift-generate
	rebar compile

compile-tests: compile
	$(ERLC) $(ERLC_TEST_FLAGS) $(TESTDIR)/*.erl

clean:
	rm -rf ebin/*.beam
	rm -f include/thrift_*.hrl
	rm -f include/scrabbleCheat_*.hrl


test: compile compile-tests
	$(ERL_START) true $(ERL_TEST_FLAGS) -run gaddag_test test $(ERL_END)
	$(ERL_START) true $(ERL_TEST_FLAGS) -run parser_test test $(ERL_END)
	$(ERL_START) true $(ERL_TEST_FLAGS) -run board_test test $(ERL_END)
	$(ERL_START) true $(ERL_TEST_FLAGS) -run movesearch_test test $(ERL_END)
	$(ERL_START) true $(ERL_TEST_FLAGS) -run followstruct_test test $(ERL_END)
	$(ERL_START) true $(ERL_TEST_FLAGS) -run move_test test $(ERL_END)
	rm -f $(BUILD)/gaddag.dict
	$(ERL_START) true $(ERL_TEST_FLAGS) -run server_test test $(ERL_END)

all: compile-all test run


shell: compile
	$(ERL_START) $(ERL_TEST_FLAGS) $(ERL_INCLUDE_FLAGS)




