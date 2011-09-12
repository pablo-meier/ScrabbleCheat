
CC=g++

compile: compile-thrift 
	$(CC) -c -I/usr/local/include/thrift -I./bin/gen-cpp src/*.cpp
	mv *.o bin
	$(CC) -o bin/sc_http -lthrift -O2 bin/*.o

compile-thrift: generate-thrift
	$(CC) -c -I/usr/local/include/thrift bin/gen-cpp/*.cpp
	mv *.o bin

generate-thrift: prepare
	thrift -r --gen cpp ../../../../lib/ScrabbleCheat.thrift
	rm -f gen-cpp/*.skeleton.cpp
	mv gen-cpp bin

clean-thrift:
	rm -rf gen-cpp

prepare: clean-thrift
	[[ -d bin ]] || mkdir -p bin

clean:
	rm -rf bin
