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

-module(move).

-import(tile, [get_tile_location/1, is_occupied/1, get_tile_bonus/1]).
-import(board, [place_move_on_board/2, to_beginning/1, orthogonals/1, get_adjacent/3, zoom/3, flip/1]).
-import(lists, [foldl/3, filter/2]).
-import(tile, [get_tile_letter/1, is_wildcard/1]).
-export([new_move/0, add_to_move/2, duplicate_moves/2, get_move_tiles/1, score/2]).

%% The move datatype.  Checks structural integrity of moves, not
%% responsible for legal placement relative to a board, or dictionary
%% checks.  Implemented simply as a list of tuples.


%% new_move :: () -> Move
%%
%% The atom prevents move from being flattened.
new_move() -> {move, []}.


%% add_to_move :: Tile * Move -> Move
%%
%% Adds to the move, or throws an error.
add_to_move(Tile, Move) ->
	{move, MoveList} = Move,
	{Row, Col} = get_tile_location(Tile),
	WithinBounds = check_integrity(Row, Col),
	case WithinBounds of
		true -> 
				{move, [Tile|MoveList]};
		_False -> 
			throw({out_of_bounds, {Row, Col}})
	end.


%% get_move_tiles :: Move -> [Tile]
%%
%% Evaluates to the tiles that compose the move.
get_move_tiles({move, MoveList}) -> MoveList.


%% duplicate_moves :: Move * Move -> Bool
%%
%% Given 2 moves, checks if they add the same tiles in the same places.
%% Note that while it is prefereable to not generate duplicates in the 
%% first place, this might be a TODO for later.
duplicate_moves({move, MoveList1}, {move, MoveList2}) ->
	lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, MoveList2) end, MoveList1) andalso
	lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, MoveList1) end, MoveList2).


%% %% check_integrity :: Int * Int -> Bool
check_integrity(Row, Col) ->
	Row =< 15 andalso Row >= 1 andalso Col =< 15 andalso Col >= 1.


%% score :: Move -> Int
%%
%% Calculates the score of a move.
score(Move, Board) ->
	Lst = get_move_tiles(Move),
	ZoomBackDir = to_beginning(get_move_orientation(Lst)),
	[ATile|_] = Lst,
	MockBoard = place_move_on_board(Move, Board),

	StartTile = zoom(ATile, ZoomBackDir, MockBoard),
	Totaled = traverse(StartTile, flip(ZoomBackDir), MockBoard),
	BonusPoints = get_bonus_points(StartTile, flip(ZoomBackDir), Lst, MockBoard),
	Totaled + BonusPoints.


%% get_move_orientation :: [Tile] -> horizontal | vertical
get_move_orientation([H|T]) ->
	{Row1, _} = get_tile_location(H),
	Horizontal = lists:all(fun (X) -> {Row2,_} = get_tile_location(X), Row2 =:= Row1 end, T),
	case Horizontal of
		true -> horizontal;
		_Else -> vertical
	end.


%% traverse :: Tile * Direction * Board -> Points 
traverse(Tile, Direction, Board) ->
	%% get the points for the row/col.  If you have a neighbor, recur.  Else, return
	NeighborZoom = to_beginning(hd(orthogonals(Direction))),
	TopOfOrthogonal = zoom(Tile, NeighborZoom, Board),
	PointValue = traverse_and_add(TopOfOrthogonal, flip(NeighborZoom), Board),
	case get_adjacent(Tile, Board, Direction) of
		none -> PointValue; 
		NewTile -> 
			case is_occupied(NewTile) of
				true ->
					PointValue + traverse(NewTile, Direction, Board);
				_Else ->
					PointValue
			end
	end.
	

%% traverse_and_add :: Tile * Direction * Board -> Points
traverse_and_add(Tile, Direction, Board) ->
	TilePoints = case is_wildcard(Tile) of true -> 0; false -> letter_score(get_tile_letter(Tile)) end,
	case get_adjacent(Tile, Board, Direction) of
		none -> TilePoints;
		NewTile ->
			case is_occupied(NewTile) of
				true -> TilePoints + traverse_and_add(NewTile, Direction, Board);
				_Else -> TilePoints
			end
	end.


get_bonus_points(StartTile, Direction, List, Board) ->
	foldl(fun (X, Y) ->
			handle_bonus(get_tile_bonus(X), X, StartTile, Direction, Board)	+ Y
		end, 0, List).


%% Handles each tile for it's bonus value.  If there is no bonus, we add no additional points.
handle_bonus(none, _, _, _, _) -> 0;
handle_bonus(double_letter_score, Tile, _, _, _) -> letter_score(get_tile_letter(Tile));
handle_bonus(triple_letter_score, Tile, _, _, _) -> 2 * letter_score(get_tile_letter(Tile));

handle_bonus(double_word_score, _, StartTile, Direction, Board) -> 
	traverse_and_add(StartTile, Direction, Board);
handle_bonus(triple_word_score, _, StartTile, Direction, Board) -> 
	2 * traverse_and_add(StartTile, Direction, Board).


%% Currently hardcoded for standard Scrabble.  Could have just defined as a list?
%% Lazy Sunday coding, this is...
letter_score($A) -> 1;  letter_score($B) -> 3;  letter_score($C) -> 3;  letter_score($D) -> 2;
letter_score($E) -> 1;  letter_score($F) -> 4;  letter_score($G) -> 2;  letter_score($H) -> 4;
letter_score($I) -> 1;  letter_score($J) -> 8;  letter_score($K) -> 5;  letter_score($L) -> 1;
letter_score($M) -> 3;  letter_score($N) -> 1;  letter_score($O) -> 1;  letter_score($P) -> 3;
letter_score($Q) -> 10; letter_score($R) -> 1;  letter_score($S) -> 1;  letter_score($T) -> 1;
letter_score($U) -> 1;  letter_score($V) -> 4;  letter_score($W) -> 4;  letter_score($X) -> 8;
letter_score($Y) -> 4;  letter_score($Z) -> 10.
