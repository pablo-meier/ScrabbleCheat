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

-define(BOARD_HEIGHT, 15).

-import(tile, [get_tile_letter/1, 
			get_tile_bonus/1,
			get_tile_location/1, 
			set_tile_letter/2,
			set_tile_bonus/2]).

-import(move, [get_move_tiles/1]).

-export([place_bonus_on_board/4,
		place_letter_on_board/4,
		place_move_on_board/2,
		get_tile/3,
		print_board/1,
		get_adjacent/3,
		get_adjacents/2,
		place_word/4,
		as_list/1]).

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

%% place_move_on_board :: Move * Board -> Board
%%
%% Returns a board with a full move placed on it.
place_move_on_board(Move, Board) ->
	Tiles = get_move_tiles(Move),
	lists:foldl(fun (Tile, AccBoard) ->
					{Row, Col} = get_tile_location(Tile),
					Char = get_tile_letter(Tile),
					place_letter_on_board(Row, Col, Char, AccBoard)
				end, Board, Tiles).

%% get_tile :: Int * Int * Board -> Tile
get_tile(Row, Col, Board) ->
	if 
		Row < 1 orelse Col < 1 orelse Row > ?BOARD_HEIGHT orelse Col > ?BOARD_HEIGHT ->
			throw({tile_request_out_of_bounds, Row, Col});
		true ->
			{RowIndex, ColIndex} = make_array_indices(Row, Col),
			RowArray = array:get(RowIndex, Board),
			array:get(ColIndex, RowArray)
	end.

%% get_adjacent :: Tile * atom(Direction) -> Tile | none
%% 
%% Returns the tile that is adjacent to the parametrized tile,
%% in the direction specified by the paramter (left, down, right, up).
%% Returns none if the tile is a border case.
get_adjacent(Tile, Board, left) -> 
	{Row, Col} = get_tile_location(Tile),
	if 
		Col =:= 1 -> 
			none;
		Col < 16 ->
			get_tile(Row, Col - 1, Board)
	end;
get_adjacent(Tile, Board, right) -> 
	{Row, Col} = get_tile_location(Tile),
	if 
		Col =:= 15 -> 
			none;
		Col > 0 ->
			get_tile(Row, Col + 1, Board)
	end;
get_adjacent(Tile, Board, up) -> 
	{Row, Col} = get_tile_location(Tile),
	if 
		Row =:= 1 -> 
			none; 
		Row < 16 -> 
			get_tile(Row - 1, Col, Board)
	end;
get_adjacent(Tile, Board, down) -> 
	{Row, Col} = get_tile_location(Tile),
	if 
		Row =:= 15 -> 
			none;
		Row > 0 ->
			get_tile(Row + 1, Col, Board)
	end.


%% get_adjacents :: Tile -> [Tile]
%%
%% Returns all ajacent tiles to the parametrized one.
get_adjacents(Tile, Board) ->
	AllAdjacents = lists:map(fun (X) -> get_adjacent(Tile, Board, X) end, [left,down,up,right]),
	lists:filter(fun (X) -> X =/= none end, AllAdjacents).


%% as_list :: Board -> [[Tile]]
%%
%% Returns the board as a list of lists.
as_list(Board) ->
	lists:map(fun (X) -> array:to_list(X) end, array:to_list(Board)).


%% place_word :: String * Direction * {Int, Int} * Board -> Board
%%
%% Direction :: right | down
%% Places a word in the specified direction on the board.
place_word([], _, _, Board) -> Board;
place_word(Word, Direction, {Row, Col}, Board) ->
	[H|T] = Word,
	case Direction of 
		down ->
			NewBoard = place_letter_on_board(Row, Col, H, Board),
			place_word(T, down, {Row + 1, Col}, NewBoard);
		right ->
			NewBoard = place_letter_on_board(Row, Col, H, Board),
			place_word(T, right, {Row, Col + 1}, NewBoard)
	end.


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
