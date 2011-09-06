/* Copyright (c) 2010 Paul Meier
* 
* Permission is hereby granted, free of charge, to any person obtaining a copy
* of this software and associated documentation files (the "Software"), to deal
* in the Software without restriction, including without limitation the rights
* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
* copies of the Software, and to permit persons to whom the Software is
* furnished to do so, subject to the following conditions:
* 
* The above copyright notice and this permission notice shall be included in
* all copies or substantial portions of the Software.
* 
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
* THE SOFTWARE.
*/

#include <stdio.h> 
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "to_binary.h"
#include "gaddag.h"
#include "constants.h"

// We traverse the gaddag, writing binary representations into a singly-linked
// list. Then we traverse the list and write it out to a file, sequentially.

typedef uint8_t uint8;
typedef uint32_t uint32;

typedef struct list_node_t {
	struct list_node_t* next;
	uint8* bin_rep;          // The actual representation of the node
	unsigned bin_size;       // size of the bin_rep, in bytes.
	unsigned self_reference; // address of this node
	gaddag* associated_gaddag;
} list_node;

static unsigned Master_Byte_Offset = 0;
static list_node* Tail_Node;
static unsigned Num_Nodes = 1;
static unsigned Node_Id = 0;
static const unsigned Max_Number_Children = 27;


/******************************************************************************
 *                             HELPER DECLARATIONS                            *
 ******************************************************************************/ 

void gaddag_to_list(list_node*);
void out_to_file(list_node*, const char*);

uint8 get_num_keys(gaddag*);
uint8 key_from_offset(unsigned);
list_node* new_list_node(gaddag*);
void finish_list_node(list_node*, gaddag*, uint32);
unsigned get_gaddag_malloc_size(gaddag*);
void release_list(list_node*);
void print_list(list_node*);

/******************************************************************************
 *                               PUBLIC INTERFACE                             *
 ******************************************************************************/ 
void
gaddag_to_binary(gaddag* input, const char* outfile)
{

	list_node* root = new_list_node(input);
	Tail_Node = root;
	
	gaddag_to_list(root);
	out_to_file(root, outfile);
	release_list(root);
}


/******************************************************************************
 *                                    HELPERS                                 *
 ******************************************************************************/ 


// Note that this function had the necessary preconditions:
//   * this_node is already allocated, and has it's self-reference set by the
//     calling function.
// As a postcondition, this function:
//   * Allocates a list_node for every child of the input gaddag.
//   * Calls itself on all children of the input gaddag
//
// This function mostly serves to write the key values to each key's list_rep,
// and as a primary instigator of the recursion case.
void
gaddag_to_list(list_node* this_node)
{
	gaddag* input = this_node->associated_gaddag;
	uint8* bin_rep = this_node->bin_rep;

	list_node* children[Max_Number_Children];
	memset(children, 0, sizeof(list_node*) * Max_Number_Children);
	unsigned child_index = 0;

	// We start writing to the binary representation after we've already written
	// the number of keys.
	uint32 bin_rep_offset = sizeof(uint8);

	unsigned i;
	for (i = 0; i < NUM_GADDAG_PTRS; ++i)
	{
		gaddag* curr = *(input->ptrs + i);
		// For each valid key
		if (curr)
		{
			// Set parent to point to child
			uint8 key = key_from_offset(i);

			// Malloc a child.
			list_node* child = new_list_node(curr);
			uint32 value = child->self_reference;

			memcpy(bin_rep + bin_rep_offset, &key, sizeof(uint8));
			bin_rep_offset += sizeof(uint8);

			memcpy(bin_rep + bin_rep_offset, &value, sizeof(uint32));
			bin_rep_offset += sizeof(uint32);

			// Link child to most previously allocated item in list.
			Tail_Node->next = child;
			Tail_Node = child;

			//Increment the number of children we cycle through in the recursion case.
			children[child_index] = child;
			++child_index;
		}
	}

	finish_list_node(this_node, input, bin_rep_offset);

	uint8 j;
	for (j = 0; j < child_index; ++j)
	{
		list_node* curr = children[j];
		gaddag_to_list(curr);		
	}
}


// Mallocs all the members, and fills in the 'immediate' data, before the keys
// (num_keys, etc.).  Returns the self-reference to the new member.
list_node*
new_list_node(gaddag* input)
{
	list_node* node = (list_node*) malloc(sizeof(list_node));
	++Node_Id;

	unsigned malloc_size = get_gaddag_malloc_size(input);
	uint8* bin_rep = (uint8*) malloc(malloc_size);
	memset(bin_rep, 0, malloc_size);

	node->bin_rep = bin_rep;
	node->bin_size = malloc_size;

	uint8 bin_num_keys = get_num_keys(input);
	memcpy(bin_rep, &bin_num_keys, sizeof(uint8));

	node->self_reference = Master_Byte_Offset;
	node->associated_gaddag = input;

	Master_Byte_Offset += malloc_size;

	return node;
}


// Writes the final values, after keys have been written and assigned.
void
finish_list_node(list_node* node, gaddag* input, uint32 bin_rep_offset)
{
	uint8* bin_rep = node->bin_rep;	

	// Write whether we are a terminator or not.
	if (input->is_terminator)
	{
		uint8 max = UINT8_MAX;
		memcpy(bin_rep + bin_rep_offset, &max, sizeof(uint8));
	}
	// else case is 0 from previous memset...
}


// 'keys' is a char[] of undetermined size passed in by the caller that we fill
// in with values. 'number' is set to the number of distinct keys.
uint8
get_num_keys(gaddag* input)
{
	unsigned number = 0;

	unsigned i;
	for(i = 0; i < NUM_GADDAG_PTRS; ++i)
	{
		gaddag* curr = *(input->ptrs + i);
		if (curr)
		{
			++number;
		}
	}
	return number;
}

unsigned
get_gaddag_malloc_size(gaddag* input)
{
	unsigned num_keys = get_num_keys(input);

	unsigned node_len_sz      = sizeof(uint8); 
	unsigned ptr_sz           = num_keys * (sizeof(uint32) + sizeof(uint8));
	unsigned is_terminator_sz = sizeof(uint8);

	unsigned malloc_size = node_len_sz + ptr_sz + is_terminator_sz;
	
	return malloc_size;
}


// Primarily used for debugging output
void
print_list(list_node* input)
{
	gaddag* gaddag = input->associated_gaddag;
	uint8* bin_rep = input->bin_rep;

	printf("--------------- WRITING OUT ----------------\n");
	printf("Binary size: %u\n", input->bin_size);
	printf("self_reference: %u\n", input->self_reference);
	printf("Number of reported keys: %u\n", get_num_keys(gaddag));
	printf("Binary representation:\n");

	unsigned i;
	for (i = 0; i < input->bin_size; ++i)
	{
		printf("%x ", *(bin_rep + i));
		
	}
	printf("\n");
	printf("--------------------------------------------\n");
}


void
out_to_file(list_node* lst, const char* outfile)
{
	FILE* fp = fopen(outfile, "w");
	if (!fp)
	{
		fprintf(stderr, "Outfile %s failed to open\n", outfile);
	}

	while(lst)
	{
		if (DEBUG) ++Num_Nodes;
		fwrite(lst->bin_rep, sizeof(uint8), lst->bin_size, fp);	
		lst = lst->next;
	}

	if (DEBUG) printf("Number of states in this GADDAG = %u\n", Num_Nodes);
	fclose(fp);
}

uint8
key_from_offset(unsigned offset)
{
	uint8 key;
	switch (offset)
	{
		case SEPARATOR_OFFSET:
			key = SEPARATOR;
			break;
		default:
			key = offset + 65;
			break;
	}
	return key;
}

void
release_list(list_node* this_node)
{
	if (this_node)
	{
		if (this_node->bin_rep)
		{
			free(this_node->bin_rep);
		}
		free(this_node);
	}
}
