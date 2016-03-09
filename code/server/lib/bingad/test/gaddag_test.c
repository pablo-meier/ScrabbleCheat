#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <check.h>

#include "../src/gaddag.h"
#include "../src/parser.h"
#include "../src/constants.h"

unsigned contains_keys(gaddag*, char*);


///////////////////////////////////////////////////////////////////////////////
//  TESTS
///////////////////////////////////////////////////////////////////////////////

START_TEST ( add_to_gaddag_test )
{
	gaddag* root = create_gaddag();

	char* str = "PAUL";
	char* reps = "P&AULAP&ULUAP&LLUAP&";

	add_to_gaddag(root, str, reps);

	char* root_keys = "PAUL";
	fail_unless( contains_keys(root, root_keys) );

//
//        _____ P -> & -> A -> U -> L
//       /
// ROOT o------ A -> P -> & -> U -> L
//      |\_____ U -> A -> P -> & -> L
//       \_____ L -> U -> A -> P -> &

	gaddag* p1_branch = (root->ptrs)[15];
	gaddag* a1_branch = (root->ptrs)[0];
	gaddag* u1_branch = (root->ptrs)[20];
	gaddag* l1_branch = (root->ptrs)[11];

	fail_unless( contains_keys(p1_branch, "&") );
	fail_unless( contains_keys(a1_branch, "P") );
	fail_unless( contains_keys(u1_branch, "A") );
	fail_unless( contains_keys(l1_branch, "U") );

	fail_unless( !p1_branch->is_terminator );
	fail_unless( !a1_branch->is_terminator );
	fail_unless( !u1_branch->is_terminator );
	fail_unless( !l1_branch->is_terminator );



	gaddag* p2_branch = (p1_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* a2_branch = (a1_branch->ptrs)[15];
	gaddag* u2_branch = (u1_branch->ptrs)[0];
	gaddag* l2_branch = (l1_branch->ptrs)[20];

	fail_unless( contains_keys(p2_branch, "A") );
	fail_unless( contains_keys(a2_branch, "&") );
	fail_unless( contains_keys(u2_branch, "P") );
	fail_unless( contains_keys(l2_branch, "A") );

	fail_unless( !p2_branch->is_terminator );
	fail_unless( !a2_branch->is_terminator );
	fail_unless( !u2_branch->is_terminator );
	fail_unless( !l2_branch->is_terminator );



	gaddag* p3_branch = (p2_branch->ptrs)[0];
	gaddag* a3_branch = (a2_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* u3_branch = (u2_branch->ptrs)[15];
	gaddag* l3_branch = (l2_branch->ptrs)[0];

	fail_unless( contains_keys(p3_branch, "U") );
	fail_unless( contains_keys(a3_branch, "U") );
	fail_unless( contains_keys(u3_branch, "&") );
	fail_unless( contains_keys(l3_branch, "P") );

	fail_unless( !p3_branch->is_terminator );
	fail_unless( !a3_branch->is_terminator );
	fail_unless( !u3_branch->is_terminator );
	fail_unless( !l3_branch->is_terminator );



	gaddag* p4_branch = (p3_branch->ptrs)[20];
	gaddag* a4_branch = (a3_branch->ptrs)[20];
	gaddag* u4_branch = (u3_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* l4_branch = (l3_branch->ptrs)[15];

	fail_unless( contains_keys(p4_branch, "L") );
	fail_unless( contains_keys(a4_branch, "L") );
	fail_unless( contains_keys(u4_branch, "L") );
	fail_unless( contains_keys(l4_branch, "&") );

	fail_unless( !p4_branch->is_terminator );
	fail_unless( !a4_branch->is_terminator );
	fail_unless( !u4_branch->is_terminator );
	fail_unless( !l4_branch->is_terminator );



	gaddag* p5_branch = (p4_branch->ptrs)[11];
	gaddag* a5_branch = (a4_branch->ptrs)[11];
	gaddag* u5_branch = (u4_branch->ptrs)[11];
	gaddag* l5_branch = (l4_branch->ptrs)[SEPARATOR_OFFSET];

	fail_unless( contains_keys(p5_branch, "") );
	fail_unless( contains_keys(a5_branch, "") );
	fail_unless( contains_keys(u5_branch, "") );
	fail_unless( contains_keys(l5_branch, "") );

	fail_unless( p5_branch->is_terminator );
	fail_unless( a5_branch->is_terminator );
	fail_unless( u5_branch->is_terminator );
	fail_unless( l5_branch->is_terminator );


	delete_gaddag(root);
}
END_TEST


START_TEST ( multiple_terminator_test )
{
	gaddag* root = create_gaddag();

	char* str = "PAUL";
	char* reps = "P&AULAP&ULUAP&LLUAP&";

	add_to_gaddag(root, str, reps);

	reps = "S&PAMPS&AMAPS&MMAPS&";
	add_to_gaddag(root, "SPAM", reps);

	reps = "D&OOD&";
	add_to_gaddag(root, "DO", reps);

	reps = "S&PAPS&AAPS&";
	add_to_gaddag(root, "SPA", reps);

// CAN'T ASCII.  Figure it out!

	char* root_keys = "SPAMULOD";
	fail_unless( contains_keys(root, root_keys) );

	// We continue without the 'AP' branches, since those
	// are the ones that diverge.  We test those after the
	// single-track ones.

	gaddag* s1_branch = (root->ptrs)[18];
	gaddag* m1_branch = (root->ptrs)[12];
	gaddag* u1_branch = (root->ptrs)[20];
	gaddag* l1_branch = (root->ptrs)[11];
	gaddag* o1_branch = (root->ptrs)[14];
	gaddag* d1_branch = (root->ptrs)[3];

	fail_unless( contains_keys(s1_branch, "&") );
	fail_unless( contains_keys(m1_branch, "A") );
	fail_unless( contains_keys(u1_branch, "A") );
	fail_unless( contains_keys(l1_branch, "U") );
	fail_unless( contains_keys(o1_branch, "D") );
	fail_unless( contains_keys(d1_branch, "&") );

	fail_unless( !s1_branch->is_terminator );
	fail_unless( !m1_branch->is_terminator );
	fail_unless( !u1_branch->is_terminator );
	fail_unless( !l1_branch->is_terminator );
	fail_unless( !o1_branch->is_terminator );
	fail_unless( !d1_branch->is_terminator );


	gaddag* s2_branch = (s1_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* m2_branch = (m1_branch->ptrs)[0];
	gaddag* u2_branch = (u1_branch->ptrs)[0];
	gaddag* l2_branch = (l1_branch->ptrs)[20];
	gaddag* o2_branch = (o1_branch->ptrs)[3];
	gaddag* d2_branch = (d1_branch->ptrs)[SEPARATOR_OFFSET];

	fail_unless( contains_keys(s2_branch, "P") );
	fail_unless( contains_keys(m2_branch, "P") );
	fail_unless( contains_keys(u2_branch, "P") );
	fail_unless( contains_keys(l2_branch, "A") );
	fail_unless( contains_keys(o2_branch, "&") );
	fail_unless( contains_keys(d2_branch, "O") );

	fail_unless( !s2_branch->is_terminator );
	fail_unless( !m2_branch->is_terminator );
	fail_unless( !u2_branch->is_terminator );
	fail_unless( !l2_branch->is_terminator );
	fail_unless( !o2_branch->is_terminator );
	fail_unless( !d2_branch->is_terminator );


	gaddag* s3_branch = (s2_branch->ptrs)[15];
	gaddag* m3_branch = (m2_branch->ptrs)[15];
	gaddag* u3_branch = (u2_branch->ptrs)[15];
	gaddag* l3_branch = (l2_branch->ptrs)[0];
	gaddag* o3_branch = (o2_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* d3_branch = (d2_branch->ptrs)[14];

	fail_unless( contains_keys(s3_branch, "A") );
	fail_unless( contains_keys(m3_branch, "S") );
	fail_unless( contains_keys(u3_branch, "&") );
	fail_unless( contains_keys(l3_branch, "P") );
	fail_unless( contains_keys(o3_branch, "") );
	fail_unless( contains_keys(d3_branch, "") );

	fail_unless( !s3_branch->is_terminator );
	fail_unless( !m3_branch->is_terminator );
	fail_unless( !u3_branch->is_terminator );
	fail_unless( !l3_branch->is_terminator );

	fail_unless( o3_branch->is_terminator );
	fail_unless( d3_branch->is_terminator );


	gaddag* s4_branch = (s3_branch->ptrs)[0];
	gaddag* m4_branch = (m3_branch->ptrs)[18];
	gaddag* u4_branch = (u3_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* l4_branch = (l3_branch->ptrs)[15];

	fail_unless( contains_keys(s4_branch, "M") );
	fail_unless( contains_keys(m4_branch, "&") );
	fail_unless( contains_keys(u4_branch, "L") );
	fail_unless( contains_keys(l4_branch, "&") );

	fail_unless( s4_branch->is_terminator );

	fail_unless( !m4_branch->is_terminator );
	fail_unless( !u4_branch->is_terminator );
	fail_unless( !l4_branch->is_terminator );


	gaddag* s5_branch = (s4_branch->ptrs)[12];
	gaddag* m5_branch = (m4_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* u5_branch = (u4_branch->ptrs)[11];
	gaddag* l5_branch = (l4_branch->ptrs)[SEPARATOR_OFFSET];

	fail_unless( contains_keys(s5_branch, "") );
	fail_unless( contains_keys(m5_branch, "") );
	fail_unless( contains_keys(u5_branch, "") );
	fail_unless( contains_keys(l5_branch, "") );

	fail_unless( s5_branch->is_terminator );
	fail_unless( m5_branch->is_terminator );
	fail_unless( u5_branch->is_terminator );
	fail_unless( l5_branch->is_terminator );

	delete_gaddag(root);
}
END_TEST

START_TEST ( multiple_branches_test )
{
	gaddag* root = create_gaddag();

	char* str = "PAUL";
	char* reps = "P&AULAP&ULUAP&LLUAP&";

	add_to_gaddag(root, str, reps);

	reps = "S&PAMPS&AMAPS&MMAPS&";
	add_to_gaddag(root, "SPAM", reps);

	reps = "D&OOD&";
	add_to_gaddag(root, "DO", reps);

	reps = "S&PAPS&AAPS&";
	add_to_gaddag(root, "SPA", reps);

	char* root_keys = "SPAMULOD";
	fail_unless( contains_keys(root, root_keys) );

	// Now back to our branchers!
//	gaddag* a1_branch = (root->ptrs)[0];

	gaddag* p1_branch = (root->ptrs)[15];
	fail_unless( contains_keys(p1_branch, "S&") );
	fail_unless( !p1_branch->is_terminator );


	gaddag* p2sep_branch = (p1_branch->ptrs)[SEPARATOR_OFFSET];
	gaddag* p2s_branch = (p1_branch->ptrs)[18];

	fail_unless( contains_keys(p2sep_branch, "A") );
	fail_unless( contains_keys(p2s_branch, "&") );

	fail_unless( !p2sep_branch->is_terminator );
	fail_unless( !p2s_branch->is_terminator );


	gaddag* p3sep_branch = (p2sep_branch->ptrs)[0];
	gaddag* p3s_branch = (p2s_branch->ptrs)[SEPARATOR_OFFSET];

	fail_unless( contains_keys(p3sep_branch, "U") );
	fail_unless( contains_keys(p3s_branch, "A") );

	fail_unless( !p3sep_branch->is_terminator );
	fail_unless( !p3s_branch->is_terminator );


//	gaddag* p4sep_branch = (p3sep_branch->ptrs)[20];
//	gaddag* p4s_branch = (p3s_branch->ptrs)[0];
//
//	fail_unless( contains_keys(p4sep_branch, "L") );
//	fail_unless( contains_keys(p4s_branch, "M") );
//
//	fail_unless( !p4sep_branch->is_terminator );
//	fail_unless( p4s_branch->is_terminator );



//	gaddag* p5sep_branch = (p4sep_branch->ptrs)[20];
//	gaddag* p5s_branch = (p4s_branch->ptrs)[0];
//
//	fail_unless( contains_keys(p5sep_branch, "") );
//	fail_unless( contains_keys(p5s_branch, "") );
//
//	fail_unless( p4sep_branch->is_terminator );
//	fail_unless( p4s_branch->is_terminator );


}
END_TEST



///////////////////////////////////////////////////////////////////////////////
//  UTILITIES
///////////////////////////////////////////////////////////////////////////////
char get_key_from_offset(unsigned offset);

unsigned 
contains_keys(gaddag* input, char* keys)
{
	gaddag** ptrs = input->ptrs;
	unsigned num_keys = strlen(keys);

	unsigned i;
	for (i = 0; i < NUM_GADDAG_PTRS; ++i)
	{
		char this_key = get_key_from_offset(i);	
		unsigned is_allowed = 0;

		unsigned j;
		for (j = 0; j < num_keys; ++j)
		{
			is_allowed |= (this_key == keys[j]);
		}
		
		if (ptrs[i])
		{
			if (!is_allowed) return 0;
		}
		else
		{
			if (is_allowed) return 0;
		}
	}

	return 1;
}


char
get_key_from_offset(unsigned offset)
{
	char key;
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



///////////////////////////////////////////////////////////////////////////////
//  TEST RUNNERS & MAIN
///////////////////////////////////////////////////////////////////////////////

Suite*
gaddag_suite(void)
{
	Suite* suite = suite_create("gaddag");

	TCase* tc_core = tcase_create("Main");
	tcase_add_test(tc_core, add_to_gaddag_test);
	tcase_add_test(tc_core, multiple_branches_test);
	tcase_add_test(tc_core, multiple_terminator_test);

	suite_add_tcase(suite, tc_core);

	return suite;
}
