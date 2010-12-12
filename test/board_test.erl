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

-module(board_test).
-include_lib("eunit/include/eunit.hrl").

-import(board_parser, [new_board/0]).
-import(board, [place_letter_on_board/5,
				place_bonus_on_board/4,
				place_word/4,
				serialize/1,
				deserialize/1,
				get_tile/3]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1]).



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
	
serialize_test() ->
	Board = place_word("ABLE", right, {1,1}, new_board()),
    {FirstBits, _} = lists:split(32, board:serialize(Board)),
    ?assert(FirstBits == "CAT0101|CBn0102|CLn0103|CEd0104|"),
    {_, LastBits} = lists:split(221 * 8, board:serialize(Board)),  
    ?assert(LastBits == "--d1512|--n1513|--n1514|--T1515|"),
    
    ?assert(board:deserialize(board:serialize(Board)) == Board).

