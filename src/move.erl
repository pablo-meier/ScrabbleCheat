-module(move).

-export([new_move/0, add_to_move/2]).

%% The move datatype.  Checks structural integrity of moves, not
%% responsible for legal placement relative to a board, or dictionary
%% checks.  Implemented simply as a list of tuples.


%% new_move :: () -> Move
new_move() -> [].


%% add_to_move :: {Int, Int, Char} * Move -> Move
%%
%% Returns 'fail' if the move fails a consistency check.
%% TODO:  Look up better error handling in Erlang?
add_to_move(Proposed, Move) ->
	{Row, Col, _Letter} = Proposed,
	WithinBounds = check_integrity(Row, Col),
	case WithinBounds of
		true -> 
			Adjacent = check_adjacency(Row, Col, Move),
			case Adjacent of 
				true -> [Proposed|Move];
				_False -> 
					io:format("Not adjacent to the rest of the move"),
					fail
			end;
		_False -> 
			io:format("Move at location ~p, ~p not within bounds.", [Row, Col]),
			fail
	end.


%% check_adjacency :: Int * Int * Move -> Bool
%%
%% Make sure the piece being added is along a line to another piece.
%% TODO:  Add an orientation check to make sure you can't add a horizontal piece
%% followed by a vertical one.  You can currently have a star-shaped move.
check_adjacency(Row, Col, Move) ->
	case length(Move) of
		0 -> true;
		_True -> 
			lists:any(fun ({OtherRow, OtherCol}) -> 
				RowDiff = OtherRow - Row,
				ColDiff = OtherCol - Col,
				((RowDiff =:= 1 orelse RowDiff =:= -1) andalso ColDiff =:= 0) orelse
				((ColDiff =:= 1 orelse ColDiff =:= -1) andalso RowDiff =:= 0)
			end, Move)
	end.

%% check_integrity :: Int * Int -> Bool
check_integrity(Row, Col) ->
	Row < 16 andalso Row > 0 andalso Col < 16 andalso Col > 0.
