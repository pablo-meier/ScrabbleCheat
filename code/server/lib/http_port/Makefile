
CC=g++


test: compile-tests
	bin/test_http

compile-tests: compile
	$(CC) -c -I/usr/local/include/thrift -I./bin/gen-cpp tests/*.cpp
	mv -f *.o bin
	$(CC) -o bin/test_http -lthrift -lcheck -O2 bin/forwarding_test.o bin/ScrabbleCheat.o bin/ScrabbleCheat_types.o bin/ScrabbleCheat_constants.o

compile: compile-thrift 
	$(CC) -c -I/usr/local/include/thrift -I./bin/gen-cpp src/*.cpp
	mv -f *.o bin
	$(CC) -o bin/sc_http -lthrift -O2 bin/*.o

compile-thrift: generate-thrift
	$(CC) -c -I/usr/local/include/thrift bin/gen-cpp/*.cpp
	mv -f *.o bin

generate-thrift: prepare
	[ -d bin/gen-cpp ] || thrift -r --gen cpp ../../../../lib/ScrabbleCheat.thrift
	rm -f gen-cpp/*.skeleton.cpp
	[ -d bin/gen-cpp ] || mv gen-cpp bin

clean-thrift:
	rm -rf gen-cpp

prepare: clean-thrift
	[[ -d bin ]] || mkdir -p bin

clean:
	rm -rf bin gen-cpp
