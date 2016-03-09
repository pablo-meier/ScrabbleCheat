#ifndef __GADDAG_H__
#define __GADDAG_H__

#include <stdint.h>

typedef struct gaddag_t {
	struct gaddag_t** ptrs;
	short is_terminator;
} gaddag;

// Allocate and free the memory needed for a GADDAG node.
gaddag* create_gaddag();
void delete_gaddag(gaddag*);

// Add's a word to the gaddag, given it and it's various representations.
void add_to_gaddag(gaddag*, const char*, const char*);

#endif
