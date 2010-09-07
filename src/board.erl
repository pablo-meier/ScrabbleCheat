-module(board).

-export([place_bonus_on_board/4,
		place_letter_on_board/4,
		print_board/1]).

%% The actual board datatype.  Queried lots to generate moves.

%% place_letter_on_board :: Int * Int * Char * Board -> Board
%%
%% The primary place where moves are employed, adds a letter to the board.
%% Returns 'fail' if the board location has been occupied by another letter tile.
place_letter_on_board(Row, Col, Char, Board) ->
	RowIndex = Row - 1,
	ColIndex = Col - 1,
	ThisRow = array:get(RowIndex, Board),
	case array:get(Col - 1, ThisRow) of 
		{none, Bonus} -> 
			NewRow = array:set(ColIndex, tile:new_tile(Char, Bonus), ThisRow),
			array:set(RowIndex, NewRow, Board);
		{Else, _Bonus} ->
			io:format("Tile Already occupied with a ~p~n", [Else]),
			fail
	end.


%% place_bonus_on_board :: Int * Int * Tile * Board -> Board
%%
%% Places a tile on the board, returns a new board with the
%% tile in place.  This is also where %% we enforce 1-indexing, rather than
%% indexing.  This is used in board creation, because it allows you to change a
%% tile's bonus.  For placement of letter, see place_letter_on_board/4
place_bonus_on_board(Row, Col, Bonus, Board) ->
	RowIndex = Row - 1,
	ColIndex = Col - 1,
	ThisRow = array:get(RowIndex, Board),
	NewRow = array:set(ColIndex, tile:new_tile(none, Bonus), ThisRow),
	array:set(RowIndex, NewRow, Board).


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
