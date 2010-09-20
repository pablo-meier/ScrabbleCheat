-module(move_test).
-include_lib("eunit/include/eunit.hrl").

-import(board_parser, [new_board/0]).
-import(board, [place_word/4]).
-import(movesearch, [generate_move_candidate_locations/1]). 
-import(tile, [get_tile_location/1]).



sample_board() ->
	Empty = new_board(),
	place_word("CARE", down, {7,7}, Empty).

candidate_location1_test() ->
	Lst = generate_move_candidate_locations(sample_board()),
	lists:foreach(fun (X) ->
					?assert(contains_tile(X, Lst))
					end, [{6,7},{11,7},{7,6},{7,8},{8,6},{8,8},{9,6},{9,8},{10,6},{10,8}]).



%% Test whether or not the list contains the parametrized tile.
contains_tile({Row, Col}, Lst) ->
	io:format("Attempting with ~p,~p~n", [Row,Col]),
	lists:any(fun (X) -> 
				{Row2, Col2} = get_tile_location(X),
				io:format("   Matching with ~p, ~p...~n", [Row2,Col2]),
				Row =:= Row2 andalso Col =:= Col2
			end, Lst).
