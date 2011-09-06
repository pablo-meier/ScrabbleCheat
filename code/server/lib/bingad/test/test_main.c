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
