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
			
-import(dict_parser, [parse/1]).
-import(gaddag, [has_branch/2, get_branch/2]).
-import(tile, [get_tile_letter/1, get_tile_bonus/1, get_tile_location/1]).

-import(followstruct, [make_followstruct/4, 
						flip_followstruct/2, 
						next/2, 
						get_followstruct_tile/1, 
						get_followstruct_direction/1, 
						get_followstruct_gaddag/1,
						get_followstruct_board/1]).

make_test_followstruct() ->
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Gaddag = lists:foldl(fun (X, Y) -> forward(X, Y) end, parse("test/testdict.txt"), "ELBA"),
	Tile = get_tile(7,6, Board),
	make_followstruct(Tile, left, Gaddag, Board).

forward(A, B) -> snd(get_branch(A, B)).
snd({_, S}) -> S.

flip_test() ->
	Followstruct = make_test_followstruct(),
	Board = get_followstruct_board(Followstruct),
	Flipped = flip_followstruct(Followstruct, get_tile(7, 10, Board)),

	FTile = get_followstruct_tile(Flipped),
	FDir = get_followstruct_direction(Flipped),
	FGaddag = get_followstruct_gaddag(Flipped),

	?assert(get_tile_location(FTile) =:= {7,11}),
	?assert(FDir =:= right),
	?assert(has_branch($R, FGaddag)).

flip2_test() ->
	ok.

flip3_test() ->
	ok.


next_test() ->
	Followstruct = make_test_followstruct(),
	Moved = next(Followstruct, $T),

	FTile = get_followstruct_tile(Moved),
	FDir = get_followstruct_direction(Moved),
	FGaddag = get_followstruct_gaddag(Moved),

	?assert(get_tile_location(FTile) =:= {7,5}),
	?assert(FDir =:= left),
	?assert(has_branch($&, FGaddag)).


next1_test() ->
	ok.

