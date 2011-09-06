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

#include "../test/to_binary_test.h"
// Weird to include the source files, but this way we can test 'private' 
// methods.
#include "../src/to_binary.c"

#include "../src/gaddag.h"
#include "../src/parser.h"
#include "../src/constants.h"


///////////////////////////////////////////////////////////////////////////////
//  TESTS
///////////////////////////////////////////////////////////////////////////////


START_TEST ( get_num_keys_test )
{
	gaddag* parsed = parse_gaddag("test/testdict.txt");
	fail_unless(get_num_keys(parsed) == 3);

	gaddag* a_branch = (parsed->ptrs)[0];
	fail_unless(get_num_keys(a_branch) == 1);

	gaddag* b_branch = (parsed->ptrs)[1];
	fail_unless(get_num_keys(b_branch) == 1);

	gaddag* c_branch = (parsed->ptrs)[2];
	fail_unless(get_num_keys(c_branch) == 1);

	gaddag* sep_branch = (a_branch->ptrs)[SEPARATOR_OFFSET];
	fail_unless(get_num_keys(sep_branch) == 2);

	gaddag* c_tail = (sep_branch->ptrs)[2];
	fail_unless(get_num_keys(c_tail) == 0);

	delete_gaddag(parsed);
}
END_TEST


START_TEST ( key_from_offset_test )
{
	fail_unless(key_from_offset(0) == 'A');
	fail_unless(key_from_offset(2) == 'C');
	fail_unless(key_from_offset(SEPARATOR_OFFSET) == SEPARATOR);
}
END_TEST


START_TEST ( get_gaddag_malloc_size_test )
{
	gaddag* parsed = parse_gaddag("test/testdict.txt");
	// Num_Keys = 1 byte
	// Keys = 3 * 5 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(parsed) == 17);

	gaddag* a_branch = (parsed->ptrs)[0];
	// Num_Keys = 1 byte
	// Keys = 1 * 5 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(a_branch) == 7);

	gaddag* b_branch = (parsed->ptrs)[1];
	// Num_Keys = 1 byte
	// Keys = 1 * 5 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(b_branch) == 7);

	gaddag* c_branch = (parsed->ptrs)[2];
	// Num_Keys = 1 byte
	// Keys = 1 * 5 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(c_branch) == 7);

	gaddag* sep_branch = (a_branch->ptrs)[SEPARATOR_OFFSET];
	// Num_Keys = 1 byte
	// Keys = 2 * 5 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(sep_branch) == 12);

	gaddag* c_tail = (sep_branch->ptrs)[2];
	// Num_Keys = 1 byte
	// Keys = 0 bytes
	// is_terminator = 1 byte
	fail_unless(get_gaddag_malloc_size(c_tail) == 2);
	fail_unless(c_tail->is_terminator != 0);

	delete_gaddag(parsed);
}
END_TEST



START_TEST (leaf_node_test)
{
	gaddag* parsed = parse_gaddag("test/testdict.txt");

	// A&C
	gaddag* leaf = (((((parsed->ptrs)[0])
								->ptrs)[SEPARATOR_OFFSET])
								->ptrs)[2];

	list_node* leaf_list = new_list_node(leaf);
	gaddag_to_list(leaf_list);

    // Ensure a list_node from a leaf is 2 bytes.
	fail_unless( leaf_list->bin_size == 2 );
	fail_unless( leaf_list->associated_gaddag == leaf);

	uint8_t* outbuf = leaf_list->bin_rep;

	fail_unless( outbuf[0] == (uint8_t) 0 );
	fail_unless( outbuf[1] == (uint8_t) UINT8_MAX );

	// A&B
	leaf = (((((parsed->ptrs)[0])
						->ptrs)[SEPARATOR_OFFSET])
						->ptrs)[1];

	leaf_list = new_list_node(leaf);
	gaddag_to_list(leaf_list);

	fail_unless( leaf_list->bin_size == 2 );
	fail_unless( leaf_list->associated_gaddag == leaf);

	outbuf = leaf_list->bin_rep;
	fail_unless( outbuf[0] == (uint8_t) 0 );
	fail_unless( outbuf[1] == (uint8_t) UINT8_MAX );

	// BA&
	leaf = (((((parsed->ptrs)[1])
						->ptrs)[0])
						->ptrs)[SEPARATOR_OFFSET];

	leaf_list = new_list_node(leaf);
	gaddag_to_list(leaf_list);

	fail_unless( leaf_list->bin_size == 2 );
	fail_unless( leaf_list->associated_gaddag == leaf);

	outbuf = leaf_list->bin_rep;


	fail_unless( outbuf[0] == (uint8_t) 0 );
	fail_unless( outbuf[1] == (uint8_t) UINT8_MAX );

	// CA&
	leaf = (((((parsed->ptrs)[2])
						->ptrs)[0])
						->ptrs)[SEPARATOR_OFFSET];

	leaf_list = new_list_node(leaf);
	gaddag_to_list(leaf_list);

	fail_unless( leaf_list->bin_size == 2 );
	fail_unless( leaf_list->associated_gaddag == leaf);

	outbuf = leaf_list->bin_rep;


	fail_unless( outbuf[0] == (uint8_t) 0 );
	fail_unless( outbuf[1] == (uint8_t) UINT8_MAX );

	delete_gaddag(parsed);
}
END_TEST


///////////////////////////////////////////////////////////////////////////////
//  TEST RUNNERS & MAIN
///////////////////////////////////////////////////////////////////////////////

Suite*
to_binary_suite(void)
{
	Suite* suite = suite_create("to_binary");

	TCase* tc_core = tcase_create("Helpers");
	tcase_add_test(tc_core, get_num_keys_test);
	tcase_add_test(tc_core, key_from_offset_test);
	tcase_add_test(tc_core, get_gaddag_malloc_size_test);
	tcase_add_test(tc_core, leaf_node_test);

	suite_add_tcase(suite, tc_core);

	return suite;
}


