
PROJECT_LIB=../../../lib

CC=g++


test: assemble-tests run
	ruby -C bin/tests sc_test.rb

assemble-tests: compile
	thrift --gen rb $(PROJECT_LIB)/ScrabbleCheat.thrift
	[ -d bin/tests ] || mkdir bin/tests
	cp -R tests/rb/*.rb bin/tests
	cp -R gen-rb/*.rb bin/tests
	[ -d bin/tests/lib ] || mkdir bin/tests/lib && cp -R tests/rb/lib bin/tests/lib
	rm -rf gen-rb

run: compile
	bin/sc_http &

compile: compile-thrift 
	$(CC) -c -I/usr/local/include/thrift -I./bin/gen-cpp src/*.cpp
	mv -f *.o bin
	$(CC) -o bin/sc_http -lthrift -O2 bin/ScrabbleCheat.o bin/ScrabbleCheat_constants.o bin/ScrabbleCheat_types.o bin/scrabblecheat_http.o

compile-thrift: generate-thrift
	$(CC) -c -I/usr/local/include/thrift bin/gen-cpp/*.cpp
	mv -f *.o bin

generate-thrift: prepare
	[ -d bin/gen-cpp ] || thrift -r --gen cpp $(PROJECT_LIB)/ScrabbleCheat.thrift
	rm -f gen-cpp/*.skeleton.cpp
	[ -d bin/gen-cpp ] || mv gen-cpp bin

clean-thrift:
	rm -rf gen-cpp

prepare: clean-thrift
	[[ -d bin ]] || mkdir -p bin

clean:
	rm -rf bin gen-cpp

