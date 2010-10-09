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

-module(followstruct).

-import(board, [get_adjacent/3]).
-import(movesearch, [flip/1]).
-import(gaddag, [get_branch/2, has_branch/2]).
-import(board, [place_letter_on_board/4]).
-import(tile, [get_tile_location/1]).

-export([make_followstruct/4,
		get_followstruct_tile/1,
		get_followstruct_direction/1,
		get_followstruct_gaddag/1,
		get_followstruct_board/1,
		flip_followstruct/2,
		next/2,
		can_advance/2]).

%% An intermediate data type for the construction of moves.  A 'followstruct'
%% contains all the information you need to 'follow along' the GADDAG and board.
%% It consists of 
%%    - A Tile, where you are currently situated.
%%    - A Direction, where you are headed.
%%    - A Gaddag, which you use to traverse.
%%    - A Board, which you frequently reference.
%%
%% Followstructs are created by the movesearch module.  This module simply
%% cleans out and abstracts away the details.

get_followstruct_tile({Tile, _, _, _}) -> Tile.
get_followstruct_direction({_, Direction, _, _}) -> Direction.
get_followstruct_gaddag({_, _, Gaddag, _}) -> Gaddag.
get_followstruct_board({_, _, _, Board}) -> Board.


%% make_followstruct :: Tile * Direction * Gaddag * Board -> FollowStruct
%%
%% Creates a new followstruct.  Silly at this point, but abstraction never
%% hurts, right?
make_followstruct(Tile, Direction, Gaddag, Board) -> {Tile, Direction, Gaddag, Board}.


%% flip_followstruct :: FollowStruct * Tile -> FollowStruct
%%
%% Adjusts the followstruct to 'flipping' its orientation, and accepting
%% a Zoomtile as its next cursor position.  Returns 'none' if it can't travel
%% beyond the edge of the board when it tries to flip.
flip_followstruct({_, Direction, Gaddag, Board}, ZoomTile) ->
	NewDirection = flip(Direction),
	NextTile = get_adjacent(ZoomTile, Board, NewDirection),
	case get_branch($&, Gaddag) of
		{branch, NewPath} -> 
			case NextTile of
				none -> none;
				_Else -> make_followstruct(NextTile, NewDirection, NewPath, Board)
			end;
		none -> throw(no_flip_path_in_gaddag)
	end.


%% next :: FollowStruct * Char -> FollowStruct
%%
%% Travels along the FollowStruct + Gaddag, after the 'presumable' placement
%% of a character in a move.  Returns 'none' if a move isn't present with that char.
next({Tile, Direction, Gaddag, Board}, Char) ->
	case get_branch(Char, Gaddag) of
		{branch, NextPath} ->
			NewTile = get_adjacent(Tile, Board, Direction),
			{Row, Col} = get_tile_location(Tile),
			NewBoard = place_letter_on_board(Row, Col, Char, Board),
			make_followstruct(NewTile, Direction, NextPath, NewBoard);
		none -> lolwut
	end.

%% can_advance :: Followstruct * Char -> Bool
%%
%% Returns whether or not we can legally call 'next' on this and advance appropriately.
can_advance({Tile, Direction, Gaddag, Board}, Char) ->
	HasBranch = has_branch(Char, Gaddag),
	Adjacent = get_adjacent(Tile, Board, Direction),
	HasBranch andalso Adjacent =/= none.
	
