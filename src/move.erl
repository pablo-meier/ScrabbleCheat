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

-import(tile, [get_tile_location/1]).
-export([new_move/0, add_to_move/2, duplicate_moves/2]).

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


%% duplicate_moves :: Move * Move -> Bool
%%
%% Given 2 moves, checks if they add the same tiles in the same places.
%% Note that while it is prefereable to not generate duplicates in the 
%% first place, this might be a TODO for later.
duplicate_moves({move, MoveList1}, {move, MoveList2}) ->
	lists:all(fun (X) -> lists:any(fun (Y) -> X =:= Y end, MoveList2) end, MoveList1).


%% check_adjacency :: Int * Int * Move -> Bool
%%
%% Make sure the piece being added is along a line to another piece.
%% TODO:  Add an orientation check to make sure you can't add a horizontal piece
%% followed by a vertical one.  You can currently have a star-shaped move.
%% check_adjacency(Row, Col, MoveList) ->
%% 	case length(MoveList) of
%% 		0 -> true;
%% 		_True -> 
%% 			lists:any(fun ({OtherRow, OtherCol}) -> 
%% 				RowDiff = OtherRow - Row,
%% 				ColDiff = OtherCol - Col,
%% 				((RowDiff =:= 1 orelse RowDiff =:= -1) andalso ColDiff =:= 0) orelse
%% 				((ColDiff =:= 1 orelse ColDiff =:= -1) andalso RowDiff =:= 0)
%% 			end, MoveList)
%% 	end.
%% 
%% %% check_integrity :: Int * Int -> Bool
check_integrity(Row, Col) ->
	Row =< 15 andalso Row >= 1 andalso Col =< 15 andalso Col >= 1.
