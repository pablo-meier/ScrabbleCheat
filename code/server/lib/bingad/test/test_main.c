#include <stdlib.h>

#include <check.h>

#include "../test/to_binary_test.h"
#include "../test/bintrie_test.h"
#include "../test/gaddag_test.h"

// Run all the tests.

int
main(void)
{
	Suite* first_suite = to_binary_suite();
	Suite* second_suite = gaddag_suite();
	Suite* third_suite = bintrie_suite();
	SRunner* first_runner = srunner_create(first_suite);
	SRunner* second_runner = srunner_create(second_suite);
	SRunner* third_runner = srunner_create(third_suite);

	srunner_run_all(first_runner, CK_NORMAL);
	srunner_run_all(second_runner, CK_NORMAL);
	srunner_run_all(third_runner, CK_NORMAL);

	unsigned number_failed;

	number_failed = srunner_ntests_failed(first_runner) + 
					srunner_ntests_failed(second_runner) +
					srunner_ntests_failed(third_runner);

	srunner_free(first_runner);
	srunner_free(second_runner);
	srunner_free(third_runner);

	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
