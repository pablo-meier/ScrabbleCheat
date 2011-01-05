# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com

BUILDDIR = build

BUILD_BEAM_DIR = $(BUILDDIR)/bin
BUILD_TEST_DIR = $(BUILDDIR)/tests

TESTDIR = test

SRCDIR = src
LIBDIR = lib
ERLC = erlc

THRIFT_PATH = $(LIBDIR)/ScrabbleCheat.thrift

ERL_THRIFT_INCLUDE = $(LIBDIR)/thrift-erl
ERL_THRIFT_SRC_OUTPUT = $(BUILDDIR)/gen-erl
ERL_THRIFT_BEAM_OUTPUT = $(BUILDDIR)/thrift-beam
ERL_THRIFT_INCLUDES = -I $(ERL_THRIFT_INCLUDE) -I $(ERL_THRIFT_SRC_OUTPUT)
ERL_THRIFT_COMPILE_FLAGS = -o $(ERL_THRIFT_BEAM_OUTPUT) $(ERL_THRIFT_INCLUDES)

ERLC_SRC_FLAGS = -v -o $(BUILD_BEAM_DIR) -pa $(SRCDIR) $(ERL_THRIFT_INCLUDES) -pa $(ERL_THRIFT_BEAM_OUTPUT)
ERLC_TEST_FLAGS = -v -o $(BUILD_TEST_DIR) -pa $(SRCDIR) -pa $(LIBDIR) -pz $(TESTDIR)

ERL = erl
ERL_INCLUDE_FLAGS = -pa $(BUILD_BEAM_DIR)
ERL_TEST_FLAGS = -pa $(BUILD_TEST_DIR) 
ERL_RUN_FLAGS = -noshell

ERL_FLAGS = $(ERL_INCLUDE_FLAGS) $(ERL_RUN_FLAGS)
ERL_END = -run erlang halt
ERL_RUN = $(ERL) $(ERL_FLAGS)


CLIENT_SRC=$(SRCDIR)/ncurses-ui
CLIENT_TESTS=$(TESTDIR)/ncurses-ui


test-client:
	ruby -I $(CLIENT_SRC) -I $(CLIENT_TESTS) $(CLIENT_TESTS)/client_serializable_test.rb

test: test-server test-client

test-server: compile compile-tests
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run gaddag_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run parser_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run board_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run movesearch_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run followstruct_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run move_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run serialization_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run tile_test test $(ERL_END)
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_FLAGS) -run gamestate_test test $(ERL_END)

all: compile-all test run

compile-all: binary-gaddag thrift-classes compile compile-tests

run: compile
	./start_server.sh
  
shell: compile
	$(ERL) $(ERL_TEST_FLAGS) $(ERL_INCLUDE_FLAGS)

binary-gaddag: compile
	$(ERL_RUN) -run main make_binary_gaddag $(ERL_END)

thrift-classes: prepare
	thrift -o $(BUILDDIR) --gen erl $(THRIFT_PATH)
	thrift -o $(BUILDDIR) --gen rb $(THRIFT_PATH)
	$(ERLC) $(ERL_THRIFT_COMPILE_FLAGS) $(ERL_THRIFT_SRC_OUTPUT)/*.erl

compile: prepare thrift-classes
	$(ERLC) $(ERLC_SRC_FLAGS) $(SRCDIR)/*.erl

compile-tests: 
	$(ERLC) $(ERLC_TEST_FLAGS) $(TESTDIR)/*.erl

prepare:
	test -d $(BUILDDIR) || mkdir -p $(BUILD_BEAM_DIR) $(BUILD_TEST_DIR) $(ERL_THRIFT_BEAM_OUTPUT)

clean:
	test -d $(BUILDDIR) && rm -rf $(BUILDDIR)

