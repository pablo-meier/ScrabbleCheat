# Build document for scrabble-cheat, Erlang implementation.
#   Summer 2010, pablo.a.meier@gmail.com
#
#
# TODO: Convert to OTP + rebar



THRIFT_PATH = ../../lib/ScrabbleCheat.thrift
ERL_END = -run erlang halt



test: compile 
	rebar eunit

all: compile-all test run

run: compile
	erl -pa ebin/ -pa priv/thrift-erl/ebin/ 

shell: compile
	erl -pa ebin/ -pa priv/thrift-erl/ebin/

thrift-generate: thrift-compile
	mv gen-erl/*.hrl include/
	mv gen-erl/*.beam ebin/
	cp -R priv/thrift-erl/include/*.hrl include/
	rm -rf gen-erl

thrift-compile: 
	thrift -r --gen erl $(THRIFT_PATH)
	erlc -o gen-erl gen-erl/*.erl

compile: thrift-generate
	rebar compile

binary-gaddag: compile
	erl -noshell -pa ebin/ -s scrabblecheat_main make_binary_gaddag $(ERL_END)

clean:
	rebar clean
	rm -f include/thrift_*.hrl
	rm -f include/scrabbleCheat_*.hrl
	rm -f ebin/gaddag.dict 


