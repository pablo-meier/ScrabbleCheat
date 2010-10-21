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

-module(followstruct_test).
-include_lib("eunit/include/eunit.hrl").

-import(board_parser, [new_board/0]).
-import(board, [get_tile/3, place_word/4]).
			
-import(move, [new_move/0]).
-import(dict_parser, [parse/1]).
-import(gaddag, [has_branch/2, get_branch/2, get_branch_from_string/2]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1, get_tile_location/1]).

-import(followstruct, [make_followstruct/5, 
						flip_followstruct/2, 
						next/3, 
						get_followstruct_tile/1, 
						get_followstruct_direction/1, 
						get_followstruct_gaddag/1,
						get_followstruct_board/1]).


flip_horiz_test() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Gaddag = get_branch_from_string("ELBA", parse("test/testdict.txt")),
	Tile = get_tile(7,6, Board),
	Followstruct = make_followstruct(Tile, left, Gaddag, Board, new_move()),
	Flipped = flip_followstruct(Followstruct, get_tile(7, 10, Board)),

	FTile = get_followstruct_tile(Flipped),
	FDir = get_followstruct_direction(Flipped),
	FGaddag = get_followstruct_gaddag(Flipped),

	?assert(get_tile_location(FTile) =:= {7,11}),
	?assert(FDir =:= right),
	?assert(has_branch($R, FGaddag)).

flip_vert_test() ->
	Board = place_word("TAR", down, {7,7}, new_board()),
	Gaddag = get_branch_from_string("RAT", parse("test/testdict.txt")),
	Tile = get_tile(6,7, Board),
	Followstruct = make_followstruct(Tile, up, Gaddag, Board, new_move()),
	Flipped = flip_followstruct(Followstruct, get_tile(9, 7, Board)),

	FTile = get_followstruct_tile(Flipped),
	FDir = get_followstruct_direction(Flipped),
	FGaddag = get_followstruct_gaddag(Flipped),

	?assert(get_tile_location(FTile) =:= {10,7}),
	?assert(FDir =:= down),
	?assert(has_branch($T, FGaddag)).

flip_south_border_test() ->
	Board = place_word("TANK", down, {12,1}, new_board()),
	Gaddag = get_branch_from_string("KNAT", parse("test/testdict.txt")),
	Tile = get_tile(11,1, Board),
	Followstruct = make_followstruct(Tile, up, Gaddag, Board, new_move()),
	?assertException(throw, try_to_flip_past_board_edge, flip_followstruct(Followstruct, get_tile(15, 1, Board))).


next_test() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Gaddag = get_branch_from_string("ELBA", parse("test/testdict.txt")),
	Tile = get_tile(7,6, Board),
	Followstruct = make_followstruct(Tile, left, Gaddag, Board, new_move()),
	{success, Moved, _} = next(Followstruct, $T, Gaddag),

	FTile = get_followstruct_tile(Moved),
	FDir = get_followstruct_direction(Moved),
	FGaddag = get_followstruct_gaddag(Moved),

	?assert(get_tile_location(FTile) =:= {7,5}),
	?assert(FDir =:= left),
	?assert(has_branch($&, FGaddag)).

