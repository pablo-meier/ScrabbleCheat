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

-module(movesearch_test).
-include_lib("eunit/include/eunit.hrl").

-define(TESTDICT, "test/testdict.txt").

-import(board_parser, [new_board/0]).
-import(board, [place_word/4, get_tile/3]).
-import(gaddag, [empty_gaddag/0, get_branch_from_string/2]).
-import(tile, [get_tile_location/1, new_tile/4]).
-import(dict_parser, [parse/1]).
-import(move, [new_move/0, duplicate_moves/2]).
-import(followstruct, [make_followstruct/5]).
-import(movesearch, [generate_move_candidate_locations/1, 
					get_zoomtiles/3,
					traverse_back_to_candidate/2,
					get_moves_from_candidate/4]). 



sample_board() ->
	Empty = new_board(),
	place_word("CARE", down, {7,7}, Empty).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Getting candidate locations
candidate_location1_test() ->
	Lst = generate_move_candidate_locations(sample_board()),
	lists:foreach(fun (X) ->
					?assert(contains_tile(X, Lst))
					end, [{6,7},{11,7},{7,6},{7,8},{8,6},{8,8},{9,6},{9,8},{10,6},{10,8}]).

candidate_location2_test() ->
	Board = place_word("POPPYCOCK", right, {6,2}, new_board()),
	Lst = generate_move_candidate_locations(Board),
	lists:foreach(fun (X) ->
					?assert(contains_tile(X, Lst))
					end, [{6,1},{6,11},{7,2},{7,3},{7,4},{7,5},{7,8},{7,9},{7,10},
						{5,2},{5,3},{5,4},{5,5},{5,8},{5,9},{5,10}]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Zoomtiles
zoomtile_first_test() ->
	Board = sample_board(),
	Gaddag = empty_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [{get_tile(10, 7, Board), up, Gaddag}]},
					{new_tile(none,none,7,6), [{get_tile(7, 7, Board), left, Gaddag}]},
					{new_tile(none,none,8,6), [{get_tile(8, 7, Board), left, Gaddag}]},
					{new_tile(none,none,11,7), [{get_tile(7, 7, Board), down, Gaddag}]},
					{new_tile(none,none,9,8), [{get_tile(9, 7, Board), right ,Gaddag}]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = get_zoomtiles(Candidate, Board, Gaddag),
					io:format("Candidate is ~p, Solution is ~p, Result is ~p~n", [Candidate, Solution, Result]),
					?assert(Result == Solution)
				end, SolutionPairs).

zoomtile_second_test() ->
	Board = board:place_word("BLE", right, {8, 8}, sample_board()),
	Gaddag = empty_gaddag(),
	SolutionPairs = [{new_tile(none,none,6,7), [{get_tile(10, 7, Board), up, Gaddag}]},
					{new_tile(none,none,7,6), [{get_tile(7, 7, Board), left, Gaddag}]},
					{new_tile(none,none,8,6), [{get_tile(8, 10, Board), left, Gaddag}]},
					{new_tile(none,none,8,11), [{get_tile(8, 7, Board), right, Gaddag}]},
					{new_tile(none,none,9,8), [{get_tile(9, 7, Board), right ,Gaddag},
												{get_tile(8, 8, Board), down, Gaddag}]},
					{new_tile(none,none,7,8), [{get_tile(7, 7, Board), right ,Gaddag},
												{get_tile(8, 8, Board), up, Gaddag}]}],
	lists:foreach(fun ({Candidate, Solution}) ->
					Result = get_zoomtiles(Candidate, Board, Gaddag),
					io:format("Candidate is ~p, Solution is ~p, Result is ~p~n", [Candidate, Solution, Result]),
					?assert(Result == Solution)
				end, SolutionPairs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Back to origin
back_to_origin_test() ->
%% 	Board = board:place_word("BLE", right, {8, 8}, sample_board()),
%% 	Gaddag = parse(?TESTDICT),
%% 	SolutionPairs = [{new_tile(none,none,6,7), [{{get_tile(6, 7, Board), up, none, new_move()}, ignore}]},
%% 					{new_tile(none,none,7,6), [{{get_tile(7, 6, Board), left, none, new_move()}, ignore}]},
%% 					{new_tile(none,none,8,6), [{{get_tile(8, 6, Board), left, none, new_move()}, ignore}]},
%% 					{new_tile(none,none,8,11), [{{get_tile(8, 11, Board), right, none, new_move()}, ignore}]},
%% 					{new_tile(none,none,9,8), [{{get_tile(9, 8, Board), right, none, new_move()}, ignore},
%% 												{{get_tile(9, 8, Board), down, none, new_move()}, ignore}]},
%% 					{new_tile(none,none,7,8), [{{get_tile(7, 8, Board), right, none, new_move()}, ignore},
%% 												{{get_tile(7, 8, Board), up, none, new_move()}, ignore}]}],
%% 	lists:foreach(fun ({Candidate, Solution}) ->
%% 					Result = lists:map(fun (X) -> traverse_back_to_candidate(X, Board) end, get_zoomtiles(Candidate, Board, Gaddag)),
%% 					io:format("Solution is ~p, Result is ~p~n", [Solution, Result]),
%% 					?assert(compare_origin_test_lists(Solution, Result))
%% 				end, SolutionPairs).
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Move Generation

get_move_from_candidate_test() ->
	Direction = left,
	Gaddag = get_branch_from_string("ELBA", parse(?TESTDICT)),
	Board = place_word("ABLE", right, {7,7}, new_board()),
	Candidate = get_tile(7, 6, Board),

	Followstruct = make_followstruct(Candidate, Direction, Gaddag, Board, new_move()),
	Zoomtile = get_tile(7, 10, Board),
	Rack = "TRS",
	Accum = [],
	
	Run = get_moves_from_candidate(Followstruct, Zoomtile, Rack, Accum),

	%% Should include SABLE, TABLE, ABLER, TABLES, STABLE
	Solutions = [{move, [{{character, $S}, none, {7,6}}]}, 
				{move, [{{character, $R}, none, {7,11}}]},
				{move, [{{character, $T}, none, {7,6}}]},
				{move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,11}}]},
				{move, [{{character, $T}, none, {7,6}},{{character, $S}, none, {7,5}}]}],

	io:format("Run contains ~p~n, Solutions are ~p~n", [Run, Solutions]),

	?assert(length(Run) =:= length(Solutions)),
	lists:foreach(fun (X) -> ?assert(lists:any(fun (Y) -> duplicate_moves(X, Y) end, Run)) end, Solutions).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HELPERS

%% compare_origin_test_lists(List1, List2) ->
%% 	lists:all(fun (X) -> lists:any(fun (Y) -> compare_origin_test_elements(X,Y) end, List2) end, List1).
%% 
%% %% FIXME This whole test suite is coked up from the followstruct refactor.  Refactor further!
%% compare_origin_test_elements({{Tile1, Direction1, _}, _}, {{Tile2, Direction2, _, _, _}, _}) ->
%% 	Tile1 == Tile2 andalso Direction1 == Direction2.
%% 

%% Test whether or not the list contains the parametrized tile.
contains_tile({Row, Col}, Lst) ->
	Return = lists:any(fun (X) -> 
				{Row2, Col2} = get_tile_location(X),
				Row =:= Row2 andalso Col =:= Col2
			end, Lst),
	case Return of 
		true -> true;
		_False -> io:format("~p,~p Were not present. ~n", [Row,Col])
	end.
