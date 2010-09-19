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

-module(board).

-import(tile, [get_tile_letter/1, 
			get_tile_location/1, 
			get_tile_bonus/1,
			set_tile_letter/2,
			set_tile_bonus/2]).

-export([place_bonus_on_board/4,
		place_letter_on_board/4,
		get_tile/3,
		print_board/1
%		get_adjacent/2,
%		get_adjacents/1,
		]).

%% The actual board datatype.  Queried lots to generate moves.

%% place_letter_on_board :: Int * Int * Char * Board -> Board
%%
%% The primary place where moves are employed, adds a letter to the board.
%% Returns 'fail' if the board location has been occupied by another letter tile.
place_letter_on_board(Row, Col, Char, Board) ->
	{RowIndex, ColIndex} = make_array_indices(Row, Col),
	ThisRow = array:get(RowIndex, Board),
	Tile = array:get(ColIndex, ThisRow),
	case get_tile_letter(Tile) of
		none ->
			NewRow = array:set(ColIndex, set_tile_letter(Char, Tile), ThisRow),
			array:set(RowIndex, NewRow, Board);
		Else ->
			throw({tile_already_occupied, Else})
	end.


%% place_bonus_on_board :: Int * Int * Tile * Board -> Board
%%
%% Places a tile on the board, returns a new board with the
%% tile in place.  This is also where %% we enforce 1-indexing, rather than
%% indexing.  This is used in board creation, because it allows you to change a
%% tile's bonus.  For placement of letter, see place_letter_on_board/4
place_bonus_on_board(Row, Col, Bonus, Board) ->
	{RowIndex, ColIndex} = make_array_indices(Row, Col),
	ThisRow = array:get(RowIndex, Board),
	Tile = array:get(ColIndex, ThisRow), 
	case get_tile_bonus(Tile) of 
		none ->
			NewRow = array:set(ColIndex, set_tile_bonus(Bonus, Tile), ThisRow),
			array:set(RowIndex, NewRow, Board);
		ABonus ->
			throw({tile_already_with_bonus, ABonus})
	end.


%% get_tile :: Int * Int * Board -> Tile
get_tile(Row, Col, Board) ->
	{RowIndex, ColIndex} = make_array_indices(Row, Col),
	RowArray = array:get(RowIndex, Board),
	array:get(ColIndex, RowArray).


%% print_board :: Board -> ()
%%
%% Pretty prints the board.
print_board(Board) ->
	print_key(),
	AsList = array:to_list(Board),
	PrintRow = fun (Row) ->
				lists:foreach(fun tile:print_tile/1, array:to_list(Row)),
				io:format("~n") end,
	lists:foreach(PrintRow, AsList).

print_key() ->
	io:format("~nKey (for any character 'p'):~n  *p* -> Triple Word!~n  ^p^ -> Double Word!~n  -p- -> Triple Letter!~n  _p_ -> Double Letter~n~n").


%% A little silly, but DRY...
make_array_indices(Num1, Num2) -> {Num1 - 1, Num2 - 1}.
