#!/bin/bash
#
# Starts the Erlang-powered ScrabbleCheat server.
#
# 

echo "Note that this opens an Erlang shell:  To start the server, you"
echo "must run 'server:start().'  I'm working on it..."

erl +K true -pa lib/thrift-erl/ebin -pa build/gen-erl -pa build/thrift-beam -pa build/bin 
