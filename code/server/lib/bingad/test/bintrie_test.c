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

#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <check.h>

#include "../test/bintrie_test.h"
#include "../src/gaddag.h"
#include "../src/parser.h"
#include "../src/to_binary.h"

uint8_t* file_in_buffer(const char*, const char*, FILE*);
void clean_objects(uint8_t*, FILE*);


typedef struct key_val_T
{
	uint8_t key;
	uint32_t value;
} key_val;

typedef struct gaddag_node_T
{
	uint8_t num_keys;
	key_val* keys;
	uint8_t is_terminator;
} gaddag_node;

gaddag_node* binary_to_node(uint8_t*);
void release_node(gaddag_node*);

///////////////////////////////////////////////////////////////////////////////
//  TESTS
///////////////////////////////////////////////////////////////////////////////

START_TEST ( most_simple_case )
{
	const char* infile = "test/testdict.txt";
	const char* outfile = "bin/testdict.dict";
	FILE* fp = 0;
	
	uint8_t* buffer = file_in_buffer(infile, outfile, fp);

	gaddag_node* root = binary_to_node(buffer);
	fail_unless(root->num_keys == 3);

	fail_unless(root->keys[0].key == 'A');
	fail_unless(root->keys[1].key == 'B');
	fail_unless(root->keys[2].key == 'C');
	fail_unless(root->is_terminator == 0);

	// Take the 'A' branch.
	gaddag_node* a_branch = binary_to_node(buffer + root->keys[0].value);
	fail_unless(a_branch->num_keys == 1);
	fail_unless(a_branch->is_terminator == 0);
	fail_unless(a_branch->keys[0].key == '&');

	//   Take the only branch, '&'
	gaddag_node* further = binary_to_node(buffer + a_branch->keys[0].value);
	fail_unless(further->num_keys == 2);
	fail_unless(further->is_terminator == 0);
	fail_unless(further->keys[0].key == 'B');
	fail_unless(further->keys[1].key == 'C');

	//     Take the tail branch, 'B' 
	gaddag_node* tail_branch_b = binary_to_node(buffer + further->keys[0].value);
	fail_unless(tail_branch_b->num_keys == 0);
	fail_unless(tail_branch_b->is_terminator == UINT8_MAX);

	//     Take the tail branch, 'C' 
	gaddag_node* tail_branch_c = binary_to_node(buffer + further->keys[1].value);
	fail_unless(tail_branch_c->num_keys == 0);
	fail_unless(tail_branch_c->is_terminator == UINT8_MAX);

	release_node(a_branch);
	release_node(further);
	release_node(tail_branch_b);
	release_node(tail_branch_c);



	release_node(root);
	clean_objects(buffer, fp);
}
END_TEST

///////////////////////////////////////////////////////////////////////////////
//  UTILITIES
///////////////////////////////////////////////////////////////////////////////

gaddag_node* 
binary_to_node(uint8_t* buffer)
{
	gaddag_node* node = (gaddag_node*) malloc(sizeof(gaddag_node));
	unsigned buffer_offset = 0;

	uint8_t num_keys;
	memcpy(&num_keys, buffer + buffer_offset, sizeof(uint8_t));
	buffer_offset += sizeof(uint8_t);

	key_val* key_vals = (key_val*) malloc(sizeof(key_val) * num_keys);

	uint8_t i;
	for (i = 0; i < num_keys; ++i)
	{
		uint8_t key;
		uint32_t value;
		memcpy(&key, buffer + buffer_offset, sizeof(uint8_t));
		buffer_offset += sizeof(uint8_t);
		memcpy(&value, buffer + buffer_offset, sizeof(uint32_t));
		buffer_offset += sizeof(uint32_t);

		key_val this_pair;
		this_pair.key = key;
		this_pair.value = value;
		*(key_vals + i) = this_pair;
	}

	uint8_t is_terminator;
	memcpy(&is_terminator, buffer + buffer_offset, sizeof(uint8_t));
	buffer_offset += sizeof(uint8_t);

	node->num_keys = num_keys;
	node->keys = key_vals;
	node->is_terminator = is_terminator;

	return node;
}

void
release_node(gaddag_node* node)
{
	if (node)
	{
		if (node->keys)
		{
			free(node->keys);
		}
		free(node);
	}
}

uint8_t*
file_in_buffer(const char* infile, const char* outfile, FILE* fp)
{
	gaddag* parsed = parse_gaddag(infile);
	gaddag_to_binary(parsed, outfile);
	delete_gaddag(parsed);
	
	fp = fopen(outfile, "r");

	if (!fp)
	{
		fprintf(stderr, "Error reading the file %s after creation.\n", outfile);
		fail();
	}

	fseek(fp, 0, SEEK_END);
	long lSize = ftell(fp);
	rewind(fp);
		
	uint8_t* buffer = (uint8_t*) malloc(sizeof(uint8_t) * lSize);
	if (!buffer) 
	{
		fprintf(stderr, "Error allocating into memory\n");
		fail();
	}

	size_t result = fread(buffer, 1, lSize, fp);
	if (!result)
	{
		fprintf(stderr, "Error reading contents into buffer");
	}

	return buffer;
}


void
clean_objects(uint8_t* buffer, FILE* fp)
{
	free(buffer);
	fclose(fp);
}


///////////////////////////////////////////////////////////////////////////////
//  TEST RUNNERS & MAIN
///////////////////////////////////////////////////////////////////////////////

Suite*
bintrie_suite(void)
{
	Suite* suite = suite_create("Bintrie");

	TCase* tc_core = tcase_create("Core");
	tcase_add_test(tc_core, most_simple_case);
	suite_add_tcase(suite, tc_core);

	return suite;
}

