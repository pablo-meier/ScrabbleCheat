#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gaddag.h"
#include "constants.h"

// Number of nodes * number of bytes.
#define NODE_BYTE_SIZE (4 * 29)

static unsigned offset_from_key(char);
static void set_terminator(gaddag*);

/******************************************************************************
 *                               PUBLIC INTERFACE                             *
 ******************************************************************************/ 


gaddag*
create_gaddag()
{
	// malloc the struct
	gaddag* g = (gaddag *) malloc(sizeof(gaddag));

	// malloc its pointer array
	const unsigned ptrs_size = NUM_GADDAG_PTRS * sizeof(gaddag*);
	gaddag** fresh_ptrs = (gaddag **) malloc(ptrs_size);
	memset(fresh_ptrs, 0, ptrs_size);

	// put it all together, baby
	g->ptrs = fresh_ptrs;
	g->is_terminator = 0;

	return g;
}


void
delete_gaddag(gaddag* input)
{
	gaddag** links = input->ptrs;
	int i;

	for (i = 0; i < NUM_GADDAG_PTRS; ++i)
	{
		gaddag* ptr = links[i];
		if (ptr) 
		{
			delete_gaddag(ptr);
		}
	}
	free(links);
	free(input);
}


void
add_to_gaddag(gaddag* root, const char* str, const char* representations)
{
	const unsigned length = strlen(str);
	const unsigned rep_length = length + 1;

	gaddag* working_gaddag = root;

	unsigned row, col;
	for (row = 0; row < length; ++row)
	{
		for (col = 0; col < rep_length; ++col)
		{
			const unsigned index = (row * rep_length) + col;
			const char key = representations[index];
			unsigned offset = offset_from_key(key);

			gaddag* branch = working_gaddag->ptrs[offset];

			if (!branch)
			{
				// If the key isn't present, create a new node.
				gaddag* new_node = create_gaddag();

				// Link it to this one.
				*(working_gaddag->ptrs + offset) = new_node;

				// set this to the working gaddag, and continue the loop.
				working_gaddag = new_node;

			}
			else
			{
				// If the key exists.
				// trace it, continue the loop.
				working_gaddag = branch;
			}
		}

		set_terminator(working_gaddag);
		working_gaddag = root;
	}
}


/******************************************************************************
 *                                    HELPERS                                 *
 ******************************************************************************/ 

static void
set_terminator(gaddag* input)
{
	input->is_terminator = 1;
}


static unsigned
offset_from_key(char key)
{
	unsigned offset;
	switch (key)
	{
		case SEPARATOR:
			offset = SEPARATOR_OFFSET;
			break;
		default:
			offset = key - 65;
			break;
	}
	return offset;
}


