#!/bin/bash
#
# Starts the Erlang-powered ScrabbleCheat server.

# erl +K true +Mis true -pa lib/thrift-erl/ebin -pa build/gen-erl -pa build/thrift-beam -pa build/bin # -run main start 
erl +K true -pa lib/thrift-erl/ebin -pa build/gen-erl -pa build/thrift-beam -pa build/bin 

