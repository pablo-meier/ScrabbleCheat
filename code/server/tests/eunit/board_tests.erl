%% Copyright (c) 2010 Paul Meier
%% 
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%% 
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(board_tests).
-include_lib("eunit/include/eunit.hrl").

-import(game_parser, [new_board/0]).
-import(board, [place_letter_on_board/5,
				place_bonus_on_board/4,
				place_word/4,
				get_tile/3]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1]).


-define(TEMP_FILE_NAME, "../ebin/gaddag_test.dict").

get_fixture_gaddag() ->
    bin_trie:get_root(twl06).
setup() ->
    bin_trie:start_from_file(?TEMP_FILE_NAME).
teardown() ->
    case whereis(giant_bintrie) of
        undefined -> ok;
        Else -> unregister(giant_bintrie),
                exit(Else, "Test finished")
    end.




placement_test() ->
	Board = new_board(),

	WithTripleLetterBonus = place_bonus_on_board(3, 2, triple_letter_score, Board),
	WithDoubleWordBonus = place_bonus_on_board(8, 11, double_word_score, Board),
	?assert(get_tile_bonus(get_tile(8, 11, WithDoubleWordBonus)) =:= double_word_score),
	?assert(get_tile_bonus(get_tile(3, 2, WithTripleLetterBonus)) =:= triple_letter_score),
	
	WithLetterC = place_letter_on_board(11, 10, $C, Board, false),
	WithLetterM = place_letter_on_board(13, 10, $M, Board, false),
	?assert(get_tile_letter(get_tile(11, 10, WithLetterC)) =:= $C),
	?assert(get_tile_letter(get_tile(13, 10, WithLetterM)) =:= $M).

preservation_test() ->
	Board = new_board(),
	WithDoubleWordBonus = place_bonus_on_board(13, 4, double_word_score, Board),

	AndWithC = place_letter_on_board(13, 4, $C, WithDoubleWordBonus, false),
	?assert(get_tile_bonus(get_tile(13, 4, AndWithC)) =:= double_word_score),

	?assertException(throw, {tile_already_occupied, $C}, place_letter_on_board(13, 4, $M, AndWithC, false)),
	?assertException(throw, {tile_already_with_bonus, double_word_score}, place_bonus_on_board(13, 4, triple_letter_score, AndWithC)).


verify_test() ->
    setup(),

	Board = new_board(),
	WithAble = board:place_word("ABLE", right, {7,7}, Board),
	WithIsland = board:place_word("ZYGOTE", down, {1,1}, WithAble),
	Gaddag = get_fixture_gaddag(),

	?assertException(throw, _, board:verify(WithIsland, Gaddag)),

	WithNonsenseHoriz = board:place_word("MOROCLE", right, {7,3}, Board),
	WithNonsenseVert = board:place_word("MOROCLE", down, {3,7}, Board),

	?assertException(throw, _, board:verify(WithNonsenseHoriz, Gaddag)),
	?assertException(throw, _, board:verify(WithNonsenseVert, Gaddag)),

    teardown().

