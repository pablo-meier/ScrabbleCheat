-module(board).

-export([place_in_board/4,
		print_board/1]).

%% The actual board datatype.  Queried lots to generate moves.

%% place_in_board :: Int * Int * Tile * Board -> Board
%%
%% Places a tile on the board, returns a new board with the
%% tile in place.  THIS SHOULD BE THE ONLY LOCATION WHERE 
%% MODIFICATIONS TO THE BOARD OCCUR.  This is also where
%% we enforce 1-indexing, rather than 0-indexing.
place_in_board(Row, Col, Value, Board) ->
	ThisRow = array:get(Row - 1, Board),
	NewRow = array:set(Col - 1, Value, ThisRow),
	array:set(Row - 1, NewRow, Board).


%% print_board :: Board -> ()
%%
%% Pretty prints the board.
print_board(Board) ->
	print_key(),
	AsList = array:to_list(Board),
	PrintRow = fun (Row) ->
				lists:foreach(fun tile:print_tile/1, array:to_list (Row)),
				io:format("~n") end,
	lists:foreach(PrintRow, AsList).

print_key() ->
	io:format("~nKey (for any character 'p'):~n  *p* -> Triple Word!~n ^p^ -> Double Word!~n  -p- -> Triple Letter!~n  _p_ -> Double Letter~n~n").
