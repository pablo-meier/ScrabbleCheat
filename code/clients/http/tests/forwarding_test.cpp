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
#include <unistd.h>
#include <sys/time.h>

#include <protocol/TBinaryProtocol.h>
#include <transport/THttpClient.h>
#include <transport/TTransportUtils.h>

#include <check.h>

#include <iostream>

#include "../bin/gen-cpp/ScrabbleCheat.h"

using namespace std;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace boost;

// Ensure that our forwarding works. We make an HTTP client, and ensure we
// get valid data back.

///////////////////////////////////////////////////////////////////////////////
//  TESTS
///////////////////////////////////////////////////////////////////////////////

// First test -- ensure new game with "Paul" and "Sam" comes back.
START_TEST ( simple_new_game_test )
{
	string host("127.0.0.1");
	shared_ptr<TTransport> transport(new THttpClient(host, 9090));
	shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
	ScrabbleCheatClient client(protocol);

	try
	{
		transport->open();

		vector<string> player_names;
		player_names.push_back(string("Paul"));
		player_names.push_back(string("Sam"));

		Gamestate gs;
		client.new_game(gs, player_names, GameName::SCRABBLE, Dictionary::TWL06);

		Board board = gs.board;
		Tile top_left_tile = board[0];
		Tile four_four_tile = board[47];  // Row = 4, Col = 4, 1-indexed.

        fail_unless( top_left_tile.row == 1 );
        fail_unless( top_left_tile.col == 1 );
        fail_unless( top_left_tile.type == LetterType::EMPTY );
        fail_unless( strcmp(top_left_tile.letter.c_str(), "") == 0 );
        fail_unless( top_left_tile.bonus == Bonus::TRIPLE_WORD_SCORE );

        fail_unless( four_four_tile.row == 4 );
        fail_unless( four_four_tile.col == 4 );
        fail_unless( four_four_tile.type == LetterType::EMPTY );
        fail_unless( strcmp(four_four_tile.letter.c_str(), "") == 0 );
        fail_unless( four_four_tile.bonus == Bonus::DOUBLE_WORD_SCORE );

        map<string, int16_t> scores = gs.scores;
        map<string, int16_t>::iterator iter;
        for (iter = scores.begin(); iter != scores.end(); ++iter)
        {
            string std_name = iter->first;
            int16_t score = iter->second;

            fail_unless( score == 0 );
            const char* name = std_name.c_str();
            fail_unless( strcmp(name, "Paul") == 0 || strcmp(name, "Sam") == 0 );
        }

		fail_unless( strcmp(gs.player_turn.c_str(), "Paul") == 0 );
		fail_unless( gs.history.size() == 0 );
		fail_unless( gs.game_name == GameName::SCRABBLE );
		fail_unless( gs.dict == Dictionary::TWL06 );

		transport->close();
	}
	catch (TException &tx)
	{
		fprintf(stderr, "%s\n", tx.what());
		fail();
	}
}
END_TEST


///////////////////////////////////////////////////////////////////////////////
//  TEST RUNNERS & MAIN
///////////////////////////////////////////////////////////////////////////////

Suite*
forwarding_suite(void)
{
	Suite* suite = suite_create("HTTP Forwarder");

	TCase* tc_core = tcase_create("Core");
	tcase_add_test(tc_core, simple_new_game_test );
	suite_add_tcase(suite, tc_core);

	return suite;
}


int
main(void)
{
	Suite* forwarding = forwarding_suite();
	SRunner* first_runner = srunner_create(forwarding);

	srunner_run_all(first_runner, CK_NORMAL);

	unsigned number_failed;

	number_failed = srunner_ntests_failed(first_runner);

	srunner_free(first_runner);

	return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
