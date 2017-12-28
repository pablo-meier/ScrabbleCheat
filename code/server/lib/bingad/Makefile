

CC=clang

OPTS= -Wall -Werror

build: prepare
	$(CC) $(OPTS) -O3 -c -o bin/gaddag.o src/gaddag.c
	$(CC) $(OPTS) -O3 -c -o bin/parser.o src/parser.c
	$(CC) $(OPTS) -O3 -c -o bin/main.o src/main.c
	$(CC) $(OPTS) -O3 -c -o bin/to_binary.o src/to_binary.c
	$(CC) $(OPTS) -o bin/gaddag_parser bin/parser.o bin/gaddag.o bin/main.o bin/to_binary.o

debug: clean prepare
	gcc -Wall -ggdb -c -o bin/gaddag.o src/gaddag.c
	gcc -Wall -ggdb -c -o bin/parser.o src/parser.c
	gcc -Wall -ggdb -c -o bin/main.o src/main.c
	gcc -Wall -ggdb -c  -o bin/to_binary.o src/to_binary.c
	gcc -Wall -ggdb -o bin/gaddag_parser bin/parser.o bin/gaddag.o bin/main.o bin/to_binary.o

test: build compile-test
	bin/test_bintries || echo "Failed"

compile-test: build
	$(CC) $(OPTS) -c -o bin/bintrie_test.o test/bintrie_test.c
	$(CC) $(OPTS) -c -o bin/to_binary_test.o test/to_binary_test.c
	$(CC) $(OPTS) -c -o bin/gaddag_test.o test/gaddag_test.c
	$(CC) $(OPTS) -c -o bin/test_main.o test/test_main.c
	$(CC) $(OPTS) -lcheck -o bin/test_bintries bin/test_main.o bin/gaddag_test.o bin/to_binary_test.o bin/bintrie_test.o bin/parser.o bin/gaddag.o



clean:
	rm -rf bin
	rm -rf *.dict
	rm -rf *.gaddag

prepare:
	[ -d bin ] || mkdir bin
