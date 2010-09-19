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

-module(board_parser).

-export([new_board/0]).

-import(board, [place_bonus_on_board/4]).
-import(tile, [new_tile/4]).
-import(lists, [foldl/3]).

-import(array, [new/2, set/3, get/2]).

-define(BOARD_LENGTH, 15).
-define(BOARD_HEIGHT, ?BOARD_LENGTH).


%% Parses files representing boards, either partially completed or create 
%% empty new ones.  Also generates the template for a blank board (see 
%% the text file in lib/board.txt. A board in this implementation is an array 
%% of arrays, and we access it in row-major format.  We also 1-index, contrary 
%% to native Erlang arrays.


-define(TRIPLE_WORD_SCORE_LOCATIONS, 
	[{1,1},{1,8},{1,15},{8,1},{8,15},{15,1},{15,8},{15,15}]).
-define(TRIPLE_LETTER_SCORE_LOCATIONS, 
	[{2,6},{2,10},{6,2},{6,6},{6,10},{6,14},{10,2},{10,6},{10,10},{10,14},{14,6},{14,10}]).
-define(DOUBLE_WORD_SCORE_LOCATIONS, 
	[{2,2},{2,14},{3,3},{3,13},{4,4},{4,12},{5,5},{5,11},{8,8},{11,5},{11,11},{12,4},{12,12},{13,3},
	{13,13},{14,2},{14,14}]).
-define(DOUBLE_LETTER_SCORE_LOCATIONS, 
	[{1,4},{1,12},{3,7},{3,9},{4,1},{4,8},{4,15},{7,3},{7,7},{7,9},{7,13},{8,4},{8,12},
	{9,3},{9,7},{9,9},{9,13},{12,1},{12,8},{12,15},{13,7},{13,9},{15,4},{15,12}]).

%% new_board :: () -> Board
%%
%% Generates a new, blank Scrabble board.  This includes all the bonuses, but
%% no tiles in place.
new_board() ->
	Empty = empty_board_template(),
	foldl(fun ({BonusType, Lst}, Acc) -> mass_inject(Acc, BonusType, Lst) end,
		Empty,
		[{triple_word_score, ?TRIPLE_WORD_SCORE_LOCATIONS}, {double_word_score, ?DOUBLE_WORD_SCORE_LOCATIONS},
		{triple_letter_score, ?TRIPLE_LETTER_SCORE_LOCATIONS}, {double_letter_score, ?DOUBLE_LETTER_SCORE_LOCATIONS}]).

mass_inject(Board, Type, Locations) ->
	foldl( fun (Loc, Acc) ->
			{Row, Col} = Loc,
			place_bonus_on_board(Row, Col, Type, Acc)
		end, Board, Locations).


%% empty_board_template :: () -> Array<Array<Tile>>
empty_board_template() ->
	ListOfRows = make_board_rows(0, []),
	ArrayOfRows = array:from_list(ListOfRows),
	array:fix(ArrayOfRows).


%% make_board_rows :: Int * [Array<Tile>] -> [Array<Tile>]
make_board_rows(?BOARD_HEIGHT, Accum) -> lists:reverse(Accum);
make_board_rows(Index, Accum) ->
	ArrayRow = array:from_list(make_board_columns(Index, 0, [])),
	Fixed = array:fix(ArrayRow),
	make_board_rows(1 + Index, [Fixed|Accum]).

make_board_columns(_, ?BOARD_HEIGHT, Accum) -> lists:reverse(Accum);
make_board_columns(RowNumber, ColNumber, Accum) ->
	make_board_columns(RowNumber, ColNumber + 1, [new_tile(none, none, RowNumber + 1, ColNumber + 1)|Accum]).
